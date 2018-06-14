laptop-labels
=============

Dependencies:

* Install [Stack](https://www.haskellstack.org/)
* Install libqrencode
  - On Mac, you will probably do this with [homebrew](https://brew.sh/): `brew install libqrencode`
  - On Linux (Debian/Ubuntu), you might use `apt-get install libqrencode-dev`
  - On Windows, things are you might use [vcpkg](https://github.com/Microsoft/vcpkg) `vcpkg install libqrencode:x64-windows`. You'll
    also want to ensure that your stack is configured with `extra-lib-dirs` and `extra-include-dirs` to point at your `vcpkg\installed\x64-windows` directories. For example, this is in my `C:\sr\config.yaml`:

```yaml
extra-include-dirs:
- C:\Users\etrep\Documents\GitHub\vcpkg\installed\x64-windows\include
extra-lib-dirs:
- C:\Users\etrep\Documents\GitHub\vcpkg\installed\x64-windows\lib
```

Building
--------

* `stack build`
* On Windows, you'll need to make sure that `qrencode.dll` is in the same place as the output exe, e.g. `cp "$HOME\Documents\GitHub\vcpkg\installed\x64-windows\bin\qrencode.dll" "$(stack path --local-install-root)\bin"` before you'll be able to run it correctly.

Usage
-----

* With a CSV from Meraki

```
stack exec -- laptop-labels --csv=CSVFILE.csv > labels-to-print.html
```

* From a SQLite3 DB

```
stack exec -- laptop-labels --db=SQLITEDB.sqlite3 > labels-to-print.html
```
