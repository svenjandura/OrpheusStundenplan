# OrpheusStundenplan

Programm zum Erstellen des Stundenplans und der Namensschilder für das Orpheus Seminar.

## Installation (Linux)
Du kannst entwerder versuchen, direkt die ausführbare Datei herunterzuladen, oder das Programm selbst zu kompilieren.

### Download der Binary-File
Die Binary befindet sich unter dist/build/OrpheusStundenplan

### Selbst kompilieren
* Du benötigst ghc (Glasgow Haskell Compiler) und cabal (Packet Manager von haskell). Anleitungen zur Installation gibt es [hier](https://www.haskell.org/downloads/linux).
* Außerdem genötigst du die GLPK (Gnu Linear Programming Kit) Bibliothek, die du am besten über deinen Packetmanager installierst. Du benötigst die -dev Variante, das Packet heißt dann ungefähr `libglpk-dev`.
* Clone dieses Repository: `git clone https://github.com/svenjandura/OrpheusStundenplan.git`
* In dem geclonten Repository:
    * `cabal install --only-dependencies` (evetuell ist die Option `--force-reinstalls` nötig)
    * `cabal configure`
    * `cabal build`
* Die ausführbare Datei liegt unter dist/build/OrpheusStundenplan
* Sollte beim installieren des benötigten Packets `gasp-1.0.1.0` ein Fehler auftreten, kannst du versuchen, eine ältere Version von Haskell zu verwenden (2.0.0.2 funktioniert wahrscheinlich) Die verschiedenen älteren Versionen kannst gibt es [hier](https://www.haskell.org/cabal/download.html)


## Installation (Windows)

### Download des fertigen Programms
Diese Option ist unter Windows wahrscheinlich zu bevorzugen. Kopiere `build_windows/glpk_4_65.dll` in dein System32 Verzeichnis. OrpheusStundenplan.exe kann dann in aus dem Verzeichnis, in dem `stundenplan.config` liegt, heraus gestartete werden.

### Selbst kompilieren
* Du benötigst die [Haskell Platform](https://www.haskell.org/platform/prior.html). Wichtig: Version 8.4.2 funtioniert nicht, mit Version 8.2.1 könnte es klappen.
* Clone oder downloade dieses Repository: https://github.com/svenjandura/OrpheusStundenplan.git
* Außerdem benötigst du die [GLPK Bibliothek](http://winglpk.sourceforge.net/). Kopiere die `glpk_x_yy.dll` an einen beliebigen Ort und benenne sie in `glpk.dll` um. Kpoiere sie außerdem einmal OHNE sie umzubennenen in dein System32 Verzeichnis. Kopiere außerem den `src` Ordner an einen beliebigen Ort.
* In dem Repository:
    * `cabal install --extra-lib-dirs="Pfad\zur\glpkdll" --extra-include-dirs="Pfad\zu\src" --only-dependencies --force-reinstalls`
    * `cabal configure`
    * `cabal build`
* Sollte `cabal install` ein Packet beim ersten Versuch nicht installieren können, lohnt es sich, es noch mindestens ein weiteres mal zu versuchen.
* Die ausführbare Datei liegt unter dist/build/OrpheusStundenplan

## Ausführen
* Im aktuellen Verzeichnis benötigst du eine Datei mit dem Namen `stundenplan.config`. Ein Beispiel ist die Datei in diesem Repository.
* Ändere die Daten in sundenplan.config wie gewünscht, erstelle alle benötigten Verzeichnisse und lege die Benötigten Dateien dort ab.
* Benötigt werden:
  * Die von der Website heruntergeladenen Daten. Führe dazu `download_xmls.sh` aus und kopiere die einzelnen xml-Dokumente im Ordner xmls/<Zeitstempel> an den gewünschten Ort
  * Der Globalstundenplan als xml.
  * Der Infotext auf jedem Lokalplan
  * Die Tex-Einstellungen für die Namensschild-Datei (wenn du Namensschilder erstellen möchtest)
  * Die Zwangszuweisungen der Schüler zu bestimmten Themen (wenn du das nutzen möchtest, siehe stundenplan.config)
* Beispiele dafür gibt es unter `data`
* Du kannst nun OrpheusStundenplan mit verschiendenen Argumenten ausführen
  * `OrpheusStundenplan` (Ohne Argumente): Erstellt den Stundenplan sowie tex-Dateien für Globalplan, Lokalplan und Namensschilder
  * `OrpheusStundenplan Stundenplan`: Erstellt nur den Stundenplan im programmeigenen Format an dem in der config-Datei angegebenem Verzeichnis
  * `OrpheusStundenplan TexGlobalplan`, `OrpheusStundenplan TexLokalplan`, `OrpheusStundenplan TexNamensschilder`: Erstellt die jeweiligen tex-Dateien
  * `OrpheusStundenplan BetreuerBewertungen <Ordung> <Namen>` zeigt die Bewertungen der Betreuer und kann vom Organisator als Hilfestellung zur
     Erstellung des Globalplans genutzt werden. Dabei ist `<Ordung>` entwerder `t` oder `b`, je nach dem, ob die Liste nach Themen oder nach Betreuern
     sortiert werden soll. `<Namen>` sind die Namen der Themen oder Betreuer, die angezeigt werden sollen. Ohne dises Argument werden alle angezeigt.
