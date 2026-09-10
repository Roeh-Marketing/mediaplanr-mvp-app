/* ---------------------------------------------------------------------------
   Editable-cell shim for the plan grid.

   reactable renders each spend cell as <input class="mp-cell" data-key="...">.
   reactable re-creates those nodes whenever the table re-renders, so listeners
   are DELEGATED from document rather than bound per input -- otherwise every
   re-render would silently drop them.

   The registry's status dropdowns ride the same delegation: reactable
   re-renders them too, and a second bound-per-node listener would have the same
   problem. They send `registry_status` instead of `grid_edit`.

   A change sends { key, value, nonce } to Shiny as `grid_edit`. The nonce
   guarantees the reactive fires even when the same cell is set to the same
   value twice, which is exactly what happens when a user re-types a number
   after an undo.
   --------------------------------------------------------------------------- */
(function () {
  var nonce = 0;

  function send(el) {
    if (!el.classList || !el.classList.contains("mp-cell")) return;
    var key = el.getAttribute("data-key");
    if (!key) return;

    var raw = el.value;
    // Empty means "clear this cell" -> zero, which is a real plan value.
    var val = raw === "" || raw === null ? 0 : Number(raw);

    if (!isFinite(val) || val < 0) {
      el.classList.add("mp-cell-bad");
      return;
    }
    el.classList.remove("mp-cell-bad");
    el.classList.add("mp-cell-dirty");

    nonce += 1;
    Shiny.setInputValue("grid_edit", { key: key, value: val, nonce: nonce },
                        { priority: "event" });
  }

  // A status change is a complete thought the moment it is picked -- there is
  // nothing to type -- so it commits on `change` with no staging.
  function sendStatus(el) {
    var scenario = el.getAttribute("data-scenario");
    if (!scenario) return;
    nonce += 1;
    Shiny.setInputValue("registry_status",
                        { scenario: scenario, status: el.value, nonce: nonce },
                        { priority: "event" });
  }

  // `change` fires on blur / Enter, which is the commit point a spreadsheet
  // user expects -- not on every keystroke.
  document.addEventListener("change", function (e) {
    var el = e.target;
    if (!el || !el.classList) return;
    if (el.classList.contains("mp-cell")) return send(el);
    if (el.classList.contains("mp-status")) return sendStatus(el);
  }, true);

  // Enter commits and moves focus out, matching spreadsheet behaviour.
  document.addEventListener("keydown", function (e) {
    if (e.key === "Enter" && e.target && e.target.classList &&
        e.target.classList.contains("mp-cell")) {
      e.preventDefault();
      e.target.blur();
    }
  }, true);

  // Select-all on focus so typing replaces rather than appends.
  document.addEventListener("focusin", function (e) {
    if (e.target && e.target.classList &&
        e.target.classList.contains("mp-cell")) {
      e.target.select();
    }
  });

  // Skill chips ride the same delegation. They are rendered by renderUI and so
  // are replaced whenever the page updates; a per-node listener would be lost.
  document.addEventListener("click", function (e) {
    var el = e.target && e.target.closest ? e.target.closest(".mp-skill") : null;
    if (!el) return;
    var id = el.getAttribute("data-skill");
    if (!id) return;
    nonce += 1;
    Shiny.setInputValue("skill_pick", { id: id, nonce: nonce },
                        { priority: "event" });
  });

  // After a commit the server may re-render the table; clear the dirty marks.
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("mp_clear_dirty", function (_) {
      document.querySelectorAll(".mp-cell-dirty")
        .forEach(function (n) { n.classList.remove("mp-cell-dirty"); });
    });

    // The assistant lives in a drawer, so anything that wants its attention --
    // a skill chip, a page telling you to ask -- has to be able to open it.
    Shiny.addCustomMessageHandler("mp_open_drawer", function (_) {
      var el = document.getElementById("mp_chat_drawer");
      if (el && window.bootstrap) {
        window.bootstrap.Offcanvas.getOrCreateInstance(el).show();
      }
    });
  }
})();
