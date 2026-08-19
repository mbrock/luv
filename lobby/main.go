// luv-lobby is the tailnet-only MQTT lobby for luv.
//
// It owns one tsnet node, tagged tag:luv, and hands its luv-lobby Tailscale
// Service listener directly to Mochi MQTT. The state directory is deliberately
// outside the checkout: it contains this node's durable Tailscale identity.
package main

import (
	"flag"
	"fmt"
	"log/slog"
	"net"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"

	mqtt "github.com/mochi-mqtt/server/v2"
	"github.com/mochi-mqtt/server/v2/listeners"
	"tailscale.com/tsnet"
)

const (
	defaultService = "svc:luv-lobby"
	defaultTag     = "tag:luv"
	defaultPort    = 1883
)

func defaultStateDir() string {
	if stateHome := os.Getenv("XDG_STATE_HOME"); stateHome != "" {
		return filepath.Join(stateHome, "luv", "lobby", "tsnet")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "./state/tsnet"
	}
	return filepath.Join(home, ".local", "state", "luv", "lobby", "tsnet")
}

// notifySystemd sends a service-manager notification when this process was
// started by systemd. Outside systemd, it is deliberately a no-op.
func notifySystemd(message string) error {
	socket := os.Getenv("NOTIFY_SOCKET")
	if socket == "" {
		return nil
	}
	// systemd spells Linux abstract Unix sockets with an @; net understands
	// that spelling for Unix-domain addresses.
	socket = strings.TrimPrefix(socket, "@")
	if strings.HasPrefix(os.Getenv("NOTIFY_SOCKET"), "@") {
		socket = "\x00" + socket
	}
	conn, err := net.DialUnix("unixgram", nil, &net.UnixAddr{Name: socket, Net: "unixgram"})
	if err != nil {
		return fmt.Errorf("connect to NOTIFY_SOCKET: %w", err)
	}
	defer conn.Close()
	if _, err := conn.Write([]byte(message)); err != nil {
		return fmt.Errorf("write systemd notification: %w", err)
	}
	return nil
}

func main() {
	logger := slog.New(slog.NewTextHandler(os.Stderr, nil)).With("service", "luv-lobby")
	stateDir := flag.String("state", defaultStateDir(), "durable tsnet state directory")
	hostname := flag.String("hostname", "luv-lobby", "Tailscale node hostname")
	service := flag.String("service", defaultService, "Tailscale Service name")
	port := flag.Int("port", defaultPort, "MQTT TCP port advertised by the Service")
	flag.Parse()

	if *port < 1 || *port > 65535 {
		logger.Error("invalid MQTT port", "port", *port)
		os.Exit(2)
	}
	if err := os.MkdirAll(*stateDir, 0700); err != nil {
		logger.Error("create tsnet state directory", "path", *stateDir, "error", err)
		os.Exit(1)
	}
	logger.Info("starting", "state_dir", *stateDir, "hostname", *hostname, "service_name", *service, "port", *port)

	authKey := os.Getenv("TS_AUTHKEY")
	tailnet := &tsnet.Server{
		Dir:           *stateDir,
		Hostname:      *hostname,
		AuthKey:       authKey,
		AdvertiseTags: []string{defaultTag},
		UserLogf: func(format string, args ...any) {
			logger.Info("tailscale", "component", "tsnet", "message", fmt.Sprintf(format, args...))
		},
	}
	// tsnet reads AuthKey when it starts; keep the bootstrap key out of the
	// durable broker process after that point.
	os.Unsetenv("TS_AUTHKEY")
	defer tailnet.Close()

	// tsnet hands Service connections over from a localhost proxy; the PROXY
	// header is how the peer's real tailnet address survives that hop.
	listener, err := tailnet.ListenService(*service, tsnet.ServiceModeTCP{
		Port:                 uint16(*port),
		PROXYProtocolVersion: 2,
	})
	if err != nil {
		logger.Error("attach Tailscale Service listener", "tag", defaultTag, "service_name", *service, "port", *port, "error", err)
		os.Exit(1)
	}
	defer listener.Close()

	broker := mqtt.New(&mqtt.Options{Logger: logger.With("component", "mqtt")})
	localClient, err := tailnet.LocalClient()
	if err != nil {
		logger.Error("open tsnet LocalAPI", "error", err)
		os.Exit(1)
	}
	// Tailscale is the MQTT account system: every MQTT session must resolve to
	// the Tailscale node actually backing its TCP connection.
	if err := broker.AddHook(newTailnetPrincipalHook(localClient, logger), nil); err != nil {
		logger.Error("install Tailnet principal hook", "error", err)
		os.Exit(1)
	}
	if err := broker.AddListener(listeners.NewNet("tailscale", newProxyListener(listener))); err != nil {
		logger.Error("attach Tailscale listener to MQTT broker", "error", err)
		os.Exit(1)
	}

	logger.Info("Tailscale Service listener attached", "fqdn", listener.FQDN, "port", *port, "tag", defaultTag)
	// Serve starts Mochi's listener and event-loop goroutines, then returns.
	// Keep this owner process alive until an explicit shutdown signal arrives.
	if err := broker.Serve(); err != nil {
		logger.Error("start MQTT broker", "error", err)
		os.Exit(1)
	}
	readyStatus := fmt.Sprintf("Ready: MQTT at %s:%d", listener.FQDN, *port)
	if err := notifySystemd("READY=1\nSTATUS=" + readyStatus); err != nil {
		logger.Error("notify systemd readiness", "error", err)
		os.Exit(1)
	}
	logger.Info("ready", "fqdn", listener.FQDN, "port", *port, "tag", defaultTag)

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, os.Interrupt, syscall.SIGTERM)
	sig := <-signals
	logger.Info("stopping", "signal", sig)
	if err := notifySystemd("STOPPING=1\nSTATUS=Stopping MQTT lobby"); err != nil {
		logger.Warn("notify systemd shutdown", "error", err)
	}
	if err := broker.Close(); err != nil {
		logger.Error("close MQTT broker", "error", err)
	}
	logger.Info("stopped")
}
