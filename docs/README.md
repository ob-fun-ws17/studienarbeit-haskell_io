# JpegRawOrganizer (JRO)
## Was ist der JpegRawOrganizer?
Der JpegRawOrganizer, im Folgenden JRO genannt, ist ein Programm zum Aussortieren von Dateien beispielsweise Fotos. 

Beim Fotografieren entsteht pro Foto sowohl eine RAW als auch eine JPEG Datei. Sortiert man diese bzw. löscht die schlechten Fotos, so macht man dies meist mit den JPEG Dateien. Danach müssen allerdings noch die zugehörigen RAWs gelöscht werden. Hierbei hilft der JRO, indem er die vorhandenen JPEG mit den RAW Dateien vergleicht und die RAWs zu denen das Programm kein zugehöriges JPEG findet, je nach Einstellung entweder Löscht oder in einen entsprechenden Ordner verschiebt. Da nicht jede Kamera das gleiche RAW und JPEG Format verwendet (Beispielsweise CR2, JPG oder DNG, JPEG) ist es außerdem möglich die Dateiendung manuell anzupassen. Dies ist natürlich nicht nur auf Foto Dateiformate beschränkt, so dass der JRO für beliebige Dateimengen verwendet werden kann.

## Installation und Ausführung
### Installation 
 1. git clone https://github.com/ob-fun-ws17/studienarbeit-haskell_io.git
 2. stack build
### Ausführung
 3. stack exec JpegRawOrganizer-exe <br>
### Ausführung von Tests
 3. stack test
 
## Verwendung 
### Befehle:


|Befehl|Kurzform|Argument|Beschreibung|Beispiel|
|:------|:------|:-----------|:-----|:-----|
|quit |**:q**|--|Beendet das Programm.|--|
|help |**h**|--|Gibt die Programmhilfe auf der Konsole aus.|--|
|showSettings |**sett**|--|Gibt die Aktuellen Einstellungen in der Konsole aus.|--|
|showDifferences |**diff**|--|Gibt eine Liste aller RAW Dateien, die keine zugehörige JPEG Datei haben auf der Konsole aus.|--|
|start |**--**|--|Startet das Löschen oder Verschieben der RAW Dateien, die keine zugehörige JPEG Datei haben.|--|
|RawPath |**rp**|Ein absoluter Pfad|Setzt den Pfad in dem die RAW Dateien zu finden sind.|rp /home/img/raws|
|JpegPath |**jp**|Ein absoluter Pfad|Setzt den Pfad in dem die JPEG Dateien zu finden sind.|jp /home/img/jpegs/|
|BinPath |**bp**|Ein absoluter Pfad|Setzt den Pfad, in den die RAW Datein verschoben werden.|bp /home/img/trash bag|
|RawEnding |**rend**|Eine Dateiendung|Setzt die Dateiendung der Dateien, die als RAW Dateien klassifiziert werden sollen.\*|rend .CR2|
|JpegEnding |**jend**|Eine Dateiendung|Setzt die Dateiendung der Dateien, die als JPEG Dateien klassifiziert werden sollen.\*|jend jpg|
|FlagDeleteFiles |**fdf**|[True\|False]|True wenn die RAW Dateien gelöscht werden sollen, False falls sie verschoben werden sollen.|fdf False|

\* Dateiendungen werden nicht case-sensitiv behandelt. Also ist JPG == jpg.

### Standart Einstellungen:


|Parameter|Wert|
|:-------|:-------|
|jpegPath|Aktueller Pfad des Programms|
|rawPath|Aktueller Pfad des Programms/RAW/|
|binPath|Aktueller Pfad des Programms/bin/|
|jpegEnding|.jpg|
|rawEnding|.cr2|
|deleteFiles|False|




  
    
    


## Dokumentation
[Haddock Dokumentation](./haddock/index.html)
