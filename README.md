laptop-labels
=============

Building
--------

* Install [Stack](https://www.haskellstack.org/)
* `stack build`

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
