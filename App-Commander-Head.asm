nolist

org #1000

write "f:\symbos\symcmder.exe"
READ "..\..\..\SRC-Main\SymbOS-Constants.asm"

relocate_start

App_BegCode

;### APPLICATION HEADER #######################################################

prgdatcod       equ 0           ;Länge Code-Teil (Pos+Len beliebig; inklusive Kopf!)
prgdatdat       equ 2           ;Länge Daten-Teil (innerhalb 16K Block)
prgdattra       equ 4           ;Länge Transfer-Teil (ab #C000)
prgdatorg       equ 6           ;Original-Origin
prgdatrel       equ 8           ;Anzahl Einträge Relocator-Tabelle
prgdatstk       equ 10          ;Länge Stack (Transfer-Teil beginnt immer mit Stack)
prgdatrs1       equ 12          ;*reserved* (3 bytes)
prgdatnam       equ 15          ;program name (24+1[0] chars)
prgdatflg       equ 40          ;flags (+1=16colour icon available)
prgdat16i       equ 41          ;file offset of 16colour icon
prgdatrs2       equ 43          ;*reserved* (5 bytes)
prgdatidn       equ 48          ;"SymExe10"
prgdatcex       equ 56          ;zusätzlicher Speicher für Code-Bereich
prgdatdex       equ 58          ;zusätzlicher Speicher für Data-Bereich
prgdattex       equ 60          ;zusätzlicher Speicher für Transfer-Bereich
prgdatres       equ 62          ;*reserviert* (26 bytes)
prgdatver       equ 88          ;required OS version (1.0)
prgdatism       equ 90          ;Icon (klein)
prgdatibg       equ 109         ;Icon (gross)
prgdatlen       equ 256         ;Datensatzlänge

prgpstdat       equ 6           ;Adresse Daten-Teil
prgpsttra       equ 8           ;Adresse Transfer-Teil
prgpstspz       equ 10          ;zusätzliche Prozessnummern (4*1)
prgpstbnk       equ 14          ;Bank (1-8)
prgpstmem       equ 48          ;zusätzliche Memory-Bereiche (8*5)
prgpstnum       equ 88          ;Programm-Nummer
prgpstprz       equ 89          ;Prozess-Nummer

            dw App_BegData-App_BegCode  ;length of code area
            dw App_BegTrns-App_BegData  ;length of data area
            dw App_EndTrns-App_BegTrns  ;length of transfer area
prgdatadr   dw #1000                ;Original-Origin                    POST Adresse Daten-Teil
prgtrnadr   dw relocate_count       ;Anzahl Einträge Relocator-Tabelle  POST Adresse Transfer-Teil
prgprztab   dw prgstk-App_BegTrns   ;Länge Stack                        POST Tabelle Prozesse
            dw 0                    ;*reserved*
App_BnkNum  db 0                    ;*reserved*                         POST bank number
            db "SymCommander":ds 12:db 0 ;Name
            db 1                    ;flags (+1=16c icon)
            dw prgicn16c-App_BegCode ;16 colour icon offset
            ds 5                    ;*reserved*
prgmemtab   db "SymExe10"           ;SymbOS-EXE-Kennung                 POST Tabelle Speicherbereiche
            dw 0                    ;zusätzlicher Code-Speicher
            dw 0                    ;zusätzlicher Data-Speicher
            dw 0                    ;zusätzlicher Transfer-Speicher
            ds 26                   ;*reserviert*
            db 0,4                  ;required OS version (4.0)
prgicnsml   db 2,8,8
            db #77,#00,#8f,#cc,#9f,#ff,#af,#1f,#af,#1f,#cf,#2e,#cf,#2e,#77,#cc
prgicnbig   db 6,24,24
            db #0f,#08,#00,#00,#03,#1e,#7e,#88,#00,#00,#23,#fe,#7e,#bb,#ff,#ff,#ab,#fe,#7e,#88,#00,#00,#23,#fe,#7e,#88,#00,#00,#23,#fe,#7e,#bb,#ff,#ff,#ab,#fe,#7e,#88,#00,#00,#23,#fe,#7e,#88,#00,#00,#23,#fe
            db #7e,#bb,#ff,#ff,#ab,#fe,#7e,#88,#00,#00,#23,#fe,#7e,#ff,#ff,#ff,#ef,#fe,#6f,#0f,#0f,#0f,#0f,#fe,#7f,#ff,#ff,#ff,#ff,#fe,#7f,#ff,#ff,#ff,#ff,#fe,#7f,#fc,#f0,#f0,#f1,#fe,#7f,#ed,#0f,#0f,#c7,#fe
            db #7f,#ed,#0f,#0f,#e7,#fe,#7f,#ed,#e0,#0f,#e7,#fe,#7f,#ed,#e6,#0f,#e7,#fe,#7f,#ed,#e6,#0f,#e7,#fe,#7f,#ed,#e6,#0f,#e7,#fe,#7f,#ed,#00,#0f,#e7,#fe,#b7,#ed,#0f,#0f,#e7,#fc,#f0,#f0,#f0,#f0,#f0,#f0


;*** FILEMANAGER LIBRARY USAGE
use_SyFile_STOTRN       equ 0   ;Reads or writes a number of sectors
use_SyFile_FILNEW       equ 1   ;Creates a new file and opens it
use_SyFile_FILOPN       equ 1   ;Opens an existing file
use_SyFile_FILCLO       equ 1   ;Closes an opened file
use_SyFile_FILINP       equ 1   ;Reads an amount of bytes out of an opened file
use_SyFile_FILOUT       equ 1   ;Writes an amount of bytes into an opened file
use_SyFile_FILPOI       equ 0   ;Moves the file pointer to another position
use_SyFile_FILF2T       equ 1   ;Decodes the file timestamp
use_SyFile_FILT2F       equ 1   ;Encodes the file timestamp
use_SyFile_FILLIN       equ 0   ;Reads one text line out of an opened file
use_SyFile_DIRDEV       equ 0   ;Sets the current drive
use_SyFile_DIRPTH       equ 0   ;Sets the current path
use_SyFile_DIRPRS       equ 1   ;Changes a property of a file or a directory
use_SyFile_DIRPRR       equ 1   ;Reads a property of a file or a directory
use_SyFile_DIRREN       equ 1   ;Renames a file or a directory
use_SyFile_DIRNEW       equ 1   ;Creates a new directory
use_SyFile_DIRINP       equ 1   ;Reads the content of a directory
use_SyFile_DIRDEL       equ 1   ;Deletes one or more files
use_SyFile_DIRRMD       equ 1   ;Deletes a sub directory
use_SyFile_DIRMOV       equ 1   ;Moves a file or sub directory
use_SyFile_DIRINF       equ 1   ;Returns information about one drive
use_SyFile_DEVDIR       equ 1   ;Reads the content of a directory (extended)

READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-FileManager.asm"

READ "App-Commander.asm"

App_EndTrns

relocate_table
relocate_end
