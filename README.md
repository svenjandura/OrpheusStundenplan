# OrpheusStundenplan

Programm zum Erstellen des Stundenplans und der Namensschilder für das Orpheus Seminar.

## Installation (Linux)
Du kannst entwerder versuchen, direkt die ausführbare Datei herunterzuladen, oder das Programm selbst zu kompilieren.

### Download der Binary-File
Die Binary befindet sich unter dist/build/OrpheusStundenplan

### Selbst kompilieren
* Du benötigst ghc (Glasgow Haskell Compiler) und cabal (Packet Manager von haskell). Anleitungen zur Installation gibt es [hier](https://www.haskell.org/downloads/linux).
* Clone dieses Repository: `git close https://github.com/svenjandura/OrpheusStundenplan.git`
* In dem geclonten Repository:
    * `cabal configure`
    * `cabal build`
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
