package main

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net"
	"net/netip"
	"time"
)

// tsnet's ListenService with ServiceModeTCP forwards every Service connection
// through a localhost proxy, so the accepted net.Conn's RemoteAddr is
// 127.0.0.1 and useless for WhoIs.  With PROXYProtocolVersion set, tsnet
// writes a PROXY protocol v2 header first, carrying the tailnet peer's real
// address.  proxyListener strips that header and presents the peer's address
// as the connection's RemoteAddr, which is all the principal hook needs.

var proxyV2Signature = []byte{0x0D, 0x0A, 0x0D, 0x0A, 0x00, 0x0D, 0x0A, 0x51, 0x55, 0x49, 0x54, 0x0A}

const proxyHeaderTimeout = 5 * time.Second

type proxyListener struct {
	net.Listener
}

func newProxyListener(inner net.Listener) net.Listener {
	return &proxyListener{Listener: inner}
}

func (l *proxyListener) Accept() (net.Conn, error) {
	for {
		conn, err := l.Listener.Accept()
		if err != nil {
			return nil, err
		}
		wrapped, err := wrapProxyConn(conn)
		if err != nil {
			// A connection without a valid header cannot be attributed to a
			// peer; drop it and keep serving.  A bad header is not a reason
			// to take the whole listener down.
			conn.Close()
			continue
		}
		return wrapped, nil
	}
}

// proxyConn is the connection behind a PROXY header: it reads from a buffered
// reader (which may already hold bytes that arrived with the header) and
// reports the header's source address as its remote.
type proxyConn struct {
	net.Conn
	reader *bufio.Reader
	remote net.Addr
	local  net.Addr
}

func (c *proxyConn) Read(p []byte) (int, error) { return c.reader.Read(p) }
func (c *proxyConn) RemoteAddr() net.Addr      { return c.remote }
func (c *proxyConn) LocalAddr() net.Addr {
	if c.local != nil {
		return c.local
	}
	return c.Conn.LocalAddr()
}

func wrapProxyConn(conn net.Conn) (net.Conn, error) {
	conn.SetReadDeadline(time.Now().Add(proxyHeaderTimeout))
	reader := bufio.NewReader(conn)
	src, dst, err := readProxyV2Header(reader)
	conn.SetReadDeadline(time.Time{})
	if err != nil {
		return nil, err
	}
	pc := &proxyConn{Conn: conn, reader: reader, remote: conn.RemoteAddr()}
	if src.IsValid() {
		pc.remote = net.TCPAddrFromAddrPort(src)
	}
	if dst.IsValid() {
		pc.local = net.TCPAddrFromAddrPort(dst)
	}
	return pc, nil
}

// readProxyV2Header consumes one PROXY protocol v2 header from r and returns
// the source and destination it names.  For a LOCAL command (health checks
// from the proxy itself) both addresses are the zero value.
func readProxyV2Header(r io.Reader) (src, dst netip.AddrPort, err error) {
	var fixed [16]byte
	if _, err := io.ReadFull(r, fixed[:]); err != nil {
		return src, dst, fmt.Errorf("read PROXY header: %w", err)
	}
	if !bytes.Equal(fixed[:12], proxyV2Signature) {
		return src, dst, errors.New("no PROXY v2 signature")
	}
	if fixed[12]>>4 != 2 {
		return src, dst, fmt.Errorf("PROXY version %d, want 2", fixed[12]>>4)
	}
	command := fixed[12] & 0x0F
	family := fixed[13] >> 4
	length := int(binary.BigEndian.Uint16(fixed[14:16]))
	body := make([]byte, length)
	if _, err := io.ReadFull(r, body); err != nil {
		return src, dst, fmt.Errorf("read PROXY addresses: %w", err)
	}
	if command == 0 { // LOCAL: the proxy speaks for itself.
		return src, dst, nil
	}
	if command != 1 {
		return src, dst, fmt.Errorf("PROXY command %d", command)
	}
	switch family {
	case 1: // AF_INET
		if length < 12 {
			return src, dst, errors.New("short IPv4 PROXY addresses")
		}
		src = netip.AddrPortFrom(netip.AddrFrom4([4]byte(body[0:4])), binary.BigEndian.Uint16(body[8:10]))
		dst = netip.AddrPortFrom(netip.AddrFrom4([4]byte(body[4:8])), binary.BigEndian.Uint16(body[10:12]))
	case 2: // AF_INET6
		if length < 36 {
			return src, dst, errors.New("short IPv6 PROXY addresses")
		}
		src = netip.AddrPortFrom(netip.AddrFrom16([16]byte(body[0:16])), binary.BigEndian.Uint16(body[32:34]))
		dst = netip.AddrPortFrom(netip.AddrFrom16([16]byte(body[16:32])), binary.BigEndian.Uint16(body[34:36]))
	default:
		return src, dst, fmt.Errorf("PROXY address family %d", family)
	}
	return src, dst, nil
}
