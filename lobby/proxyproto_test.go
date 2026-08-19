package main

import (
	"bytes"
	"encoding/binary"
	"io"
	"net"
	"net/netip"
	"testing"
)

func proxyV2Header(command byte, src, dst netip.AddrPort) []byte {
	var body []byte
	family := byte(0)
	if command == 1 {
		if src.Addr().Is4() {
			family = 1
			s, d := src.Addr().As4(), dst.Addr().As4()
			body = append(body, s[:]...)
			body = append(body, d[:]...)
		} else {
			family = 2
			s, d := src.Addr().As16(), dst.Addr().As16()
			body = append(body, s[:]...)
			body = append(body, d[:]...)
		}
		body = binary.BigEndian.AppendUint16(body, src.Port())
		body = binary.BigEndian.AppendUint16(body, dst.Port())
	}
	header := append([]byte(nil), proxyV2Signature...)
	header = append(header, 0x20|command, family<<4|1)
	header = binary.BigEndian.AppendUint16(header, uint16(len(body)))
	return append(header, body...)
}

func TestReadProxyV2Header(t *testing.T) {
	src4 := netip.MustParseAddrPort("100.121.253.4:49016")
	dst4 := netip.MustParseAddrPort("100.125.20.210:1883")
	src6 := netip.MustParseAddrPort("[fd7a:115c:a1e0::1]:5000")
	dst6 := netip.MustParseAddrPort("[fd7a:115c:a1e0::b437:14d3]:1883")

	for name, tc := range map[string]struct {
		header   []byte
		src, dst netip.AddrPort
	}{
		"ipv4":  {proxyV2Header(1, src4, dst4), src4, dst4},
		"ipv6":  {proxyV2Header(1, src6, dst6), src6, dst6},
		"local": {proxyV2Header(0, netip.AddrPort{}, netip.AddrPort{}), netip.AddrPort{}, netip.AddrPort{}},
	} {
		t.Run(name, func(t *testing.T) {
			payload := []byte("MQTT follows")
			r := bytes.NewReader(append(tc.header, payload...))
			src, dst, err := readProxyV2Header(r)
			if err != nil {
				t.Fatal(err)
			}
			if src != tc.src || dst != tc.dst {
				t.Fatalf("got %v -> %v, want %v -> %v", src, dst, tc.src, tc.dst)
			}
			rest, _ := io.ReadAll(r)
			if !bytes.Equal(rest, payload) {
				t.Fatalf("header consumed the wrong amount; rest = %q", rest)
			}
		})
	}

	t.Run("garbage is refused", func(t *testing.T) {
		if _, _, err := readProxyV2Header(bytes.NewReader([]byte("\x10\x0d\x00\x04MQTT\x05\x02\x00\x3c\x00\x00\x00"))); err == nil {
			t.Fatal("a bare MQTT CONNECT was accepted as a PROXY header")
		}
	})
}

func TestProxyListenerPresentsPeerAddress(t *testing.T) {
	inner, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer inner.Close()
	listener := newProxyListener(inner)

	src := netip.MustParseAddrPort("100.121.253.4:49016")
	dst := netip.MustParseAddrPort("100.125.20.210:1883")
	go func() {
		client, err := net.Dial("tcp", inner.Addr().String())
		if err != nil {
			return
		}
		defer client.Close()
		client.Write(proxyV2Header(1, src, dst))
		client.Write([]byte("hello"))
	}()

	conn, err := listener.Accept()
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()
	if got := conn.RemoteAddr().String(); got != src.String() {
		t.Fatalf("RemoteAddr = %s, want %s", got, src)
	}
	if got := conn.LocalAddr().String(); got != dst.String() {
		t.Fatalf("LocalAddr = %s, want %s", got, dst)
	}
	buf := make([]byte, 5)
	if _, err := io.ReadFull(conn, buf); err != nil || string(buf) != "hello" {
		t.Fatalf("payload after header = %q, %v", buf, err)
	}
}
