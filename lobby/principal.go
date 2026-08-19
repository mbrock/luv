package main

import (
	"context"
	"fmt"
	"log/slog"
	"sync"
	"time"

	mqtt "github.com/mochi-mqtt/server/v2"
	"github.com/mochi-mqtt/server/v2/packets"
	"tailscale.com/client/tailscale/apitype"
)

// whoIsClient is tsnet's embedded LocalAPI, narrowed to the identity lookup we
// need. Keeping this interface small makes the admission boundary testable.
type whoIsClient interface {
	WhoIs(context.Context, string) (*apitype.WhoIsResponse, error)
}

// tailnetPrincipal is the Tailscale-authenticated identity behind an MQTT
// connection. MQTT client IDs and usernames remain client-supplied metadata;
// this is the authority used to admit and authorize the session.
type tailnetPrincipal struct {
	NodeName string
	NodeID   string
	User     string
	Tags     []string
}

type tailnetPrincipalHook struct {
	mqtt.HookBase
	local      whoIsClient
	log        *slog.Logger
	principals sync.Map // map[*mqtt.Client]tailnetPrincipal
}

func newTailnetPrincipalHook(local whoIsClient, log *slog.Logger) *tailnetPrincipalHook {
	return &tailnetPrincipalHook{local: local, log: log.With("component", "tailnet-principal")}
}

func (h *tailnetPrincipalHook) ID() string { return "tailnet-principal" }

func (h *tailnetPrincipalHook) Provides(event byte) bool {
	switch event {
	case mqtt.OnConnect, mqtt.OnConnectAuthenticate, mqtt.OnACLCheck, mqtt.OnDisconnect:
		return true
	default:
		return false
	}
}

func (h *tailnetPrincipalHook) OnConnect(cl *mqtt.Client, _ packets.Packet) error {
	if cl.Net.Conn == nil {
		return fmt.Errorf("no network connection for MQTT client %q", cl.ID)
	}
	remote := cl.Net.Conn.RemoteAddr().String()
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	who, err := h.local.WhoIs(ctx, remote)
	if err != nil {
		h.log.Warn("identity lookup failed", "client_id", cl.ID, "remote", remote, "error", err)
		return nil // authentication turns this into a normal MQTT refusal.
	}
	if who.Node == nil {
		h.log.Warn("identity lookup returned no node", "client_id", cl.ID, "remote", remote)
		return nil
	}
	principal := tailnetPrincipal{
		NodeName: who.Node.Name,
		NodeID:   string(who.Node.StableID),
		Tags:     append([]string(nil), who.Node.Tags...),
	}
	if who.UserProfile != nil {
		principal.User = who.UserProfile.LoginName
	}
	h.principals.Store(cl, principal)
	h.log.Info("MQTT client identified", "client_id", cl.ID, "remote", remote,
		"node", principal.NodeName, "node_id", principal.NodeID,
		"user", principal.User, "tags", principal.Tags)
	return nil
}

func (h *tailnetPrincipalHook) OnConnectAuthenticate(cl *mqtt.Client, _ packets.Packet) bool {
	_, ok := h.principals.Load(cl)
	if !ok {
		h.log.Warn("MQTT connection denied without Tailnet principal", "client_id", cl.ID, "remote", cl.Net.Remote)
	}
	return ok
}

func (h *tailnetPrincipalHook) OnACLCheck(cl *mqtt.Client, _ string, _ bool) bool {
	_, ok := h.principals.Load(cl)
	return ok
}

func (h *tailnetPrincipalHook) OnDisconnect(cl *mqtt.Client, err error, expire bool) {
	if value, ok := h.principals.LoadAndDelete(cl); ok {
		principal := value.(tailnetPrincipal)
		h.log.Info("MQTT client disconnected", "client_id", cl.ID, "node", principal.NodeName,
			"user", principal.User, "expire", expire, "error", err)
	}
}
