package main

import (
	"context"
	"errors"
	"io"
	"log/slog"
	"net"
	"path/filepath"
	"runtime"
	"testing"

	mqtt "github.com/mochi-mqtt/server/v2"
	"github.com/mochi-mqtt/server/v2/packets"
	"tailscale.com/client/tailscale/apitype"
	"tailscale.com/tailcfg"
)

type fakeWhoIsClient struct {
	who    *apitype.WhoIsResponse
	err    error
	remote string
}

func testLogger() *slog.Logger {
	return slog.New(slog.NewTextHandler(io.Discard, nil))
}

func (f *fakeWhoIsClient) WhoIs(_ context.Context, remote string) (*apitype.WhoIsResponse, error) {
	f.remote = remote
	return f.who, f.err
}

func TestNotifySystemd(t *testing.T) {
	socket := filepath.Join(t.TempDir(), "notify.sock")
	listener, err := net.ListenUnixgram("unixgram", &net.UnixAddr{Name: socket, Net: "unixgram"})
	if err != nil {
		t.Fatal(err)
	}
	defer listener.Close()
	t.Setenv("NOTIFY_SOCKET", socket)

	if err := notifySystemd("READY=1\nSTATUS=test"); err != nil {
		t.Fatal(err)
	}
	buffer := make([]byte, 128)
	n, _, err := listener.ReadFromUnix(buffer)
	if err != nil {
		t.Fatal(err)
	}
	if got, want := string(buffer[:n]), "READY=1\nSTATUS=test"; got != want {
		t.Errorf("notification = %q, want %q", got, want)
	}
}

func TestNotifySystemdWithoutManager(t *testing.T) {
	t.Setenv("NOTIFY_SOCKET", "")
	if err := notifySystemd("READY=1"); err != nil {
		t.Fatal(err)
	}
}

func TestNotifySystemdAbstractSocket(t *testing.T) {
	if runtime.GOOS != "linux" {
		t.Skip("systemd abstract sockets are Linux-specific")
	}
	socket := "\x00luv-lobby-systemd-notify-test"
	listener, err := net.ListenUnixgram("unixgram", &net.UnixAddr{Name: socket, Net: "unixgram"})
	if err != nil {
		t.Fatal(err)
	}
	defer listener.Close()
	t.Setenv("NOTIFY_SOCKET", "@"+socket[1:])

	if err := notifySystemd("READY=1"); err != nil {
		t.Fatal(err)
	}
	buffer := make([]byte, 128)
	n, _, err := listener.ReadFromUnix(buffer)
	if err != nil {
		t.Fatal(err)
	}
	if got, want := string(buffer[:n]), "READY=1"; got != want {
		t.Errorf("notification = %q, want %q", got, want)
	}
}

func TestLobbyBrokerRetainsValuesAcrossRestart(t *testing.T) {
	storeDir := filepath.Join(t.TempDir(), "mqtt")
	const topic = "luv/store/OPENAI_API_KEY"
	value := []byte("secret")

	broker, err := newLobbyBroker(testLogger(), storeDir, true)
	if err != nil {
		t.Fatal(err)
	}
	if got := broker.Options.Capabilities.MaximumMessageExpiryInterval; got != 0 {
		t.Fatalf("maximum message expiry = %d, want unlimited (0)", got)
	}
	if err := broker.Serve(); err != nil {
		t.Fatal(err)
	}
	if err := broker.Publish(topic, value, true, 1); err != nil {
		t.Fatal(err)
	}
	if err := broker.Close(); err != nil {
		t.Fatal(err)
	}

	restarted, err := newLobbyBroker(testLogger(), storeDir, false)
	if err != nil {
		t.Fatal(err)
	}
	defer restarted.Close()
	if err := restarted.Serve(); err != nil {
		t.Fatal(err)
	}
	messages := restarted.Topics.Messages(topic)
	if len(messages) != 1 {
		t.Fatalf("restored messages = %d, want 1", len(messages))
	}
	if got := string(messages[0].Payload); got != string(value) {
		t.Fatalf("restored payload = %q, want %q", got, value)
	}
}

func TestTailnetPrincipalAdmitsIdentifiedClient(t *testing.T) {
	local, remote := net.Pipe()
	defer local.Close()
	defer remote.Close()
	lookup := &fakeWhoIsClient{who: &apitype.WhoIsResponse{
		Node: &tailcfg.Node{
			Name:     "chapel.whale-justice.ts.net.",
			StableID: "n123",
			Tags:     []string{"tag:player"},
		},
		UserProfile: &tailcfg.UserProfile{LoginName: "mikael@brockman.se"},
	}}
	hook := newTailnetPrincipalHook(lookup, testLogger())
	client := &mqtt.Client{ID: "game-7", Net: mqtt.ClientConnection{Conn: local, Remote: "tailnet-peer"}}

	if err := hook.OnConnect(client, packets.Packet{}); err != nil {
		t.Fatal(err)
	}
	if lookup.remote == "" {
		t.Fatal("identity lookup did not receive the remote address")
	}
	if !hook.OnConnectAuthenticate(client, packets.Packet{}) {
		t.Fatal("identified Tailnet client was denied")
	}
	if !hook.OnACLCheck(client, "lobby/chat", true) {
		t.Fatal("identified Tailnet client was denied topic access")
	}
	hook.OnDisconnect(client, nil, false)
	if hook.OnACLCheck(client, "lobby/chat", true) {
		t.Fatal("disconnected client retained topic access")
	}
}

func TestTailnetPrincipalDeniesUnknownClient(t *testing.T) {
	local, remote := net.Pipe()
	defer local.Close()
	defer remote.Close()
	hook := newTailnetPrincipalHook(&fakeWhoIsClient{err: errors.New("unknown peer")}, testLogger())
	client := &mqtt.Client{ID: "unknown", Net: mqtt.ClientConnection{Conn: local, Remote: "tailnet-peer"}}

	if err := hook.OnConnect(client, packets.Packet{}); err != nil {
		t.Fatal(err)
	}
	if hook.OnConnectAuthenticate(client, packets.Packet{}) {
		t.Fatal("unknown Tailnet client was admitted")
	}
}
