/* ---------------------------------------------------------------------------
   Editable-cell shim for the plan grid.

   reactable renders each spend cell as <input class="mp-cell" data-key="...">.
   reactable re-creates those nodes whenever the table re-renders, so listeners
   are DELEGATED from document rather than bound per input -- otherwise every
   re-render would silently drop them.

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

  // `change` fires on blur / Enter, which is the commit point a spreadsheet
  // user expects -- not on every keystroke.
  document.addEventListener("change", function (e) { send(e.target); }, true);

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

  // After a commit the server may re-render the table; clear the dirty marks.
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("mp_clear_dirty", function (_) {
      document.querySelectorAll(".mp-cell-dirty")
        .forEach(function (n) { n.classList.remove("mp-cell-dirty"); });
    });
  }
})();
