// Figure cards: hovering, focusing, or tapping a #ID mention shows the
// target figure's card (built into the page) as a top-layer popover placed
// beside the mention.  No dependencies; degrades to plain links.

(function () {
  "use strict";

  var popover = null;
  var current = null;
  var hideTimer = null;
  var showTimer = null;

  function ensurePopover() {
    if (popover) return popover;
    popover = document.createElement("div");
    popover.className = "figure-popover";
    popover.setAttribute("popover", "manual");
    popover.addEventListener("mouseenter", cancelHide);
    popover.addEventListener("mouseleave", scheduleHide);
    document.body.appendChild(popover);
    return popover;
  }

  function cardFor(link) {
    if (link.dataset.card) return document.getElementById(link.dataset.card);
    var href = link.getAttribute("href") || "";
    var hash = href.indexOf("#");
    if (hash < 0) return null;
    return document.getElementById("card-" + href.slice(hash + 1));
  }

  function place(link) {
    var pop = ensurePopover();
    var rect = link.getBoundingClientRect();
    var margin = 8;
    var wide = pop.querySelector("nav.definitions") !== null;
    var width = Math.min((wide ? 40 : 34) * 16, window.innerWidth - 2 * margin);
    pop.style.width = width + "px";
    pop.style.left = "0px";
    pop.style.top = "0px";
    var height = pop.offsetHeight;
    var left = Math.max(margin, Math.min(rect.left, window.innerWidth - width - margin));
    var below = rect.bottom + 6;
    var top = below;
    if (below + height > window.innerHeight - margin && rect.top - 6 - height > margin) {
      top = rect.top - 6 - height;
    }
    pop.style.left = left + window.scrollX + "px";
    pop.style.top = top + window.scrollY + "px";
  }

  function show(link) {
    var card = cardFor(link);
    if (!card) return;
    cancelHide();
    var pop = ensurePopover();
    if (current !== link) {
      pop.innerHTML = card.innerHTML;
      current = link;
    }
    if (!pop.matches(":popover-open")) {
      try { pop.showPopover(); } catch (e) { pop.style.display = "block"; }
    }
    place(link);
  }

  function hide() {
    if (!popover) return;
    try { popover.hidePopover(); } catch (e) { popover.style.display = "none"; }
    current = null;
  }

  function scheduleHide() {
    cancelHide();
    hideTimer = setTimeout(hide, 220);
  }

  function cancelHide() {
    if (hideTimer) { clearTimeout(hideTimer); hideTimer = null; }
  }

  function isMention(node) {
    return node && node.closest && node.closest("a.mention[href], a.definition-link[href]");
  }

  function isCardButton(node) {
    return node && node.closest && node.closest("button[data-card]");
  }

  document.addEventListener("mouseover", function (event) {
    var link = isMention(event.target);
    if (!link) return;
    if (showTimer) clearTimeout(showTimer);
    showTimer = setTimeout(function () { show(link); }, 120);
  });

  document.addEventListener("mouseout", function (event) {
    var link = isMention(event.target);
    if (!link) return;
    if (showTimer) { clearTimeout(showTimer); showTimer = null; }
    scheduleHide();
  });

  document.addEventListener("focusin", function (event) {
    var link = isMention(event.target);
    if (link) show(link);
  });

  document.addEventListener("focusout", function (event) {
    if (isMention(event.target)) scheduleHide();
  });

  // On touch, the first tap shows the card; the second follows the link.
  // A card button toggles its card.
  document.addEventListener("click", function (event) {
    var button = isCardButton(event.target);
    if (button) {
      event.preventDefault();
      if (current === button && popover && popover.matches(":popover-open")) hide();
      else show(button);
      return;
    }
    var link = isMention(event.target);
    if (!link) { if (popover && !event.target.closest(".figure-popover")) hide(); return; }
    if (window.matchMedia("(hover: none)").matches && current !== link) {
      event.preventDefault();
      show(link);
    }
  });

  document.addEventListener("keydown", function (event) {
    if (event.key === "Escape") hide();
  });

  window.addEventListener("scroll", function () { if (current) place(current); }, { passive: true });
  window.addEventListener("resize", function () { if (current) place(current); });

  // Source sidebar: bring the current file into view within the sidebar's
  // own scroll box, without moving the page.
  function revealCurrentFile() {
    var nav = document.querySelector("aside.source-nav");
    var currentFile = nav && nav.querySelector("li.current");
    if (currentFile && nav.scrollTop === 0 && nav.clientHeight > 0 &&
        currentFile.offsetTop > nav.clientHeight * 0.6) {
      nav.scrollTop = currentFile.offsetTop - nav.clientHeight / 2;
    }
  }
  revealCurrentFile();
  // A tab opened in the background has no layout yet; try again when shown.
  document.addEventListener("visibilitychange", revealCurrentFile);

  // Math and diagrams: load KaTeX or Mermaid from the CDN only when the
  // page has something for them to draw.
  function loadStyle(href) {
    var link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = href;
    document.head.appendChild(link);
  }

  function loadScript(src, onload) {
    var script = document.createElement("script");
    script.src = src;
    script.defer = true;
    script.onload = onload;
    document.head.appendChild(script);
  }

  var mathNodes = document.querySelectorAll(".math");
  if (mathNodes.length) {
    loadStyle("https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css");
    loadScript("https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js", function () {
      mathNodes.forEach(function (node) {
        var tex = node.textContent;
        try {
          window.katex.render(tex, node, {
            displayMode: node.classList.contains("display"),
            throwOnError: false
          });
        } catch (e) { /* leave the TeX source visible */ }
      });
    });
  }

  if (document.querySelector("pre.mermaid")) {
    loadScript("https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.min.js", function () {
      var dark = window.matchMedia("(prefers-color-scheme: dark)").matches;
      window.mermaid.initialize({ startOnLoad: false, theme: dark ? "dark" : "neutral",
                                  fontFamily: "Public Sans, sans-serif" });
      window.mermaid.run({ nodes: document.querySelectorAll("pre.mermaid") });
    });
  }
})();
