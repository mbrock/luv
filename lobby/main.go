// luv-lobby is the tailnet-only MQTT lobby for luv.
//
// It owns one tsnet node, tagged tag:luv, and hands its luv-lobby Tailscale
// Service listener directly to Mochi MQTT. The state directory is deliberately
// outside the checkout: it contains this node's durable Tailscale identity.
package main

import (
	"flag"
	"log"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"

	mqtt "github.com/mochi-mqtt/server/v2"
	"github.com/mochi-mqtt/server/v2/hooks/auth"
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

func main() {
	stateDir := flag.String("state", defaultStateDir(), "durable tsnet state directory")
	hostname := flag.String("hostname", "luv-lobby", "Tailscale node hostname")
	service := flag.String("service", defaultService, "Tailscale Service name")
	port := flag.Int("port", defaultPort, "MQTT TCP port advertised by the Service")
	flag.Parse()

	if *port < 1 || *port > 65535 {
		log.Fatalf("invalid MQTT port %d", *port)
	}
	if err := os.MkdirAll(*stateDir, 0700); err != nil {
		log.Fatalf("create tsnet state directory: %v", err)
	}

	authKey := os.Getenv("TS_AUTHKEY")
	tailnet := &tsnet.Server{
		Dir:           *stateDir,
		Hostname:      *hostname,
		AuthKey:       authKey,
		AdvertiseTags: []string{defaultTag},
	}
	// tsnet reads AuthKey when it starts; keep the bootstrap key out of the
	// durable broker process after that point.
	os.Unsetenv("TS_AUTHKEY")
	defer tailnet.Close()

	listener, err := tailnet.ListenService(*service, tsnet.ServiceModeTCP{
		Port: uint16(*port),
	})
	if err != nil {
		log.Fatalf("listen as %s on %s:%d: %v", defaultTag, *service, *port, err)
	}
	defer listener.Close()

	broker := mqtt.New(nil)
	// Tailscale policy admits clients to this listener. MQTT itself currently
	// has no second credential or topic ACL layer; add a Mochi auth hook before
	// treating mutually untrusted tailnet members as lobby participants.
	if err := broker.AddHook(new(auth.AllowHook), nil); err != nil {
		log.Fatalf("install MQTT admission hook: %v", err)
	}
	if err := broker.AddListener(listeners.NewNet("tailscale", listener)); err != nil {
		log.Fatalf("attach Tailscale listener to MQTT broker: %v", err)
	}

	log.Printf("MQTT lobby available at %s:%d as %s", listener.FQDN, *port, defaultTag)
	// Serve starts Mochi's listener and event-loop goroutines, then returns.
	// Keep this owner process alive until an explicit shutdown signal arrives.
	if err := broker.Serve(); err != nil {
		log.Fatal(err)
	}

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, os.Interrupt, syscall.SIGTERM)
	sig := <-signals
	log.Printf("stopping on %s", sig)
	if err := broker.Close(); err != nil {
		log.Printf("close MQTT broker: %v", err)
	}
}
