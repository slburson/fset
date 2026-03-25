/* FSet book theme switcher */

(function () {
    var THEMES = ['light', 'dark', 'hc'];
    var LABELS = { light: 'Light', dark: 'Dark', hc: 'High Contrast' };
    var STORAGE_KEY = 'fset-theme';

    // Map our logical names to data-theme attribute values (light = default, no attribute).
    function applyTheme(theme) {
        var root = document.documentElement;
        if (theme === 'light') {
            root.removeAttribute('data-theme');
        } else {
            root.setAttribute('data-theme', theme);
        }
    }

    // Determine starting theme: saved preference, then prefers-color-scheme, then light.
    function initialTheme() {
        var saved = localStorage.getItem(STORAGE_KEY);
        if (saved && THEMES.indexOf(saved) !== -1) return saved;
        if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) return 'dark';
        return 'light';
    }

    // Apply immediately (before paint) to avoid flash of wrong theme.
    var current = initialTheme();
    applyTheme(current);

    // Build and inject the picker widget once the DOM is ready.
    function injectPicker() {
        var sel = document.createElement('select');
        sel.id = 'theme-switcher';
        sel.setAttribute('aria-label', 'Color theme');
        sel.title = 'Color theme';

        THEMES.forEach(function (t) {
            var opt = document.createElement('option');
            opt.value = t;
            opt.textContent = LABELS[t];
            if (t === current) opt.selected = true;
            sel.appendChild(opt);
        });

        sel.addEventListener('change', function () {
            current = sel.value;
            applyTheme(current);
            localStorage.setItem(STORAGE_KEY, current);
        });

        document.body.appendChild(sel);
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', injectPicker);
    } else {
        injectPicker();
    }
})();
