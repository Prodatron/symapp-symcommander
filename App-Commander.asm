;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                  S y m  C o m m a n d e r   (FileManager)                  @
;@                                                                            @
;@             (c) 2004-2020 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

relocate_start


;-----------------------------------
;copy files in nested subdirectories
;-----------------------------------
;pathsrc = source      path
;pathdst = destination path
;
;dim filepos(20)
;depth = 0
;
;filepos(depth) = 0
;entries = getdir(pathsrc)
;
;doit = true
;while doit
;    if len(entries) > filepos(depth):
;        file = entries(filepos(depth))
;        filepos(depth) = filepos(depth) + 1
;        if file.typ = file:
;            copy(pathsrc + "/" + file.name, pathdst + "/" + file.name)
;        else if file.typ = dir:
;            mkdir(pathdst + "/" + file.name)
;            pathsrc = pathsrc + "/" + file.name
;            pathdst = pathdst + "/" + file.name
;            depth = depth + 1
;            filepos(depth) = 0
;            entries = getdir(pathsrc)
;        endif
;    else:
;        ##remove all files in pathsrc, if this is a move command##
;        if depth > 0:
;            pathsrc = go_parent(pathsrc)
;            pathdst = go_parent(pathdst)
;            depth = depth - 1
;            entries = getdir(pathsrc)
;        else:
;            doit = false
;        endif
;    endif
;wend
;-----------------------------------

;TODO
;- recursive copy/move/delete
;- swap list doesn't update menu sorting flags in
;+ Edit Verknüpfung
;- View Verknüpfung
;- 1 selected -> button = "rename"; 1+ marked -> button = "move"
;- Verzeichnisse vergleichen
;- Search

;==============================================================================
;### CODE-TEIL ################################################################
;==============================================================================

;### PROGRAMM-KOPF ############################################################

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

prgcodbeg   dw prgdatbeg-prgcodbeg  ;Länge Code-Teil
            dw prgtrnbeg-prgdatbeg  ;Länge Daten-Teil
            dw prgtrnend-prgtrnbeg  ;Länge Transfer-Teil
prgdatadr   dw #1000                ;Original-Origin                    POST Adresse Daten-Teil
prgtrnadr   dw relocate_count       ;Anzahl Einträge Relocator-Tabelle  POST Adresse Transfer-Teil
prgprztab   dw prgstk-prgtrnbeg     ;Länge Stack                        POST Tabelle Prozesse
            dw 0                    ;*reserved*
prgbnknum   db 0                    ;*reserved*                         POST bank number
            db "SymCommander":ds 12:db 0 ;Name
            db 1                    ;flags (+1=16c icon)
            dw prgicn16c-prgcodbeg  ;16 colour icon offset
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


;### PRGPRZ -> Programm-Prozess
dskprzn     db 2
windatprz   equ 3   ;Prozeßnummer
windatsup   equ 51  ;Nummer des Superfensters+1 oder 0
prgwin      db 0    ;Nummer des Haupt-Fensters
diawin      db 0    ;Nummer des Dialog-Fensters
prgbnk      db 0    ;Bank des Programmes*16

prgprz  call SySystem_HLPINI
        ld a,(prgprzn)
        ld (prgwindat+windatprz),a
        ld (diainpwin+windatprz),a
        ld (drvprpwin+windatprz),a
        ld (properwin+windatprz),a
        ld (attribwin+windatprz),a
        ld (copmovwin+windatprz),a
        ld (configwin+windatprz),a
        ld (confrmwin+windatprz),a
        ld (ovrwrtwin+windatprz),a
        ld a,(prgbnknum)
        add a
        add a
        add a
        add a
        ld (prgbnk),a           ;Bank*16 merken
        call devini             ;vorhandene Laufwerke holen
        call cfgini             ;Config-Pfad generieren und Config laden
        ld a,(lstakt)
        or a
        call lstswp6
        call lstini             ;Speicher für Listen reservieren und vorbereiten
        jp c,prgend

        ld c,MSC_DSK_WINOPN
        ld a,(prgbnknum)
        ld b,a
        ld de,prgwindat
        call msgsnd             ;Fenster aufbauen
prgprz1 call msgdsk             ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        cp MSR_DSK_WOPNER
        jp z,prgend             ;kein Speicher für Fenster -> Prozeß beenden
        cp MSR_DSK_WOPNOK
        jr nz,prgprz1           ;andere Message als "Fenster geöffnet" -> ignorieren
        ld a,(prgmsgb+4)
        ld (prgwin),a           ;Fenster wurde geöffnet -> Nummer merken

prgprz3 xor a
        call pthupd
        ld a,1
        call pthupd
prgprz6 xor a
        call lstref
        ld a,1
        call lstref

prgprz0 call msgget
        jr nc,prgprz0
        ld e,(iy+1)
        cp MSR_DSK_CFOCUS       ;*** Focus auf anderes Control
        jr nz,prgprz7
        ld a,(prgwin)
        cp e
        jr nz,prgprz0
        ld a,(iy+3)
        dec a
        jp nz,lstfocm
        jp lstfock
prgprz7 cp MSR_DSK_WCLICK       ;*** Fenster-Aktion wurde geklickt
        jr nz,prgprz0
        ld a,(prgwin)
        cp e
        jr z,prgprz4
        ld a,(diawin)
        cp e
        jr nz,prgprz0
        ld a,(iy+2)             ;*** DIALOG-FENSTER
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,diainpc
        jr prgprz5
prgprz4 ld a,(iy+2)             ;*** HAUPT-FENSTER
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,prgend
        cp DSK_ACT_KEY          ;*** Taste wurde gedrückt
        jr z,prgkey
prgprz5 cp DSK_ACT_MENU         ;*** Menü wurde geklickt
        jr z,prgprz2
        cp DSK_ACT_CONTENT      ;*** Inhalt wurde geklickt
        jr nz,prgprz0
prgprz2 ld l,(iy+8)
        ld h,(iy+9)
        ld a,h
        or l
        jr z,prgprz0
        ld a,(iy+3)             ;A=Klick-Typ (0/1/2=Maus links/rechts/doppelt, 7=Tastatur)
        jp (hl)

;### PRGKEY -> Taste auswerten
prgkeya equ 25
prgkeyt db 141:dw hlpopn    ;F1     = Help
        db 142:dw fildrv    ;F2     = Drive Info
        db 145:dw cmdcop    ;F5     = Copy
        db 146:dw cmdmov    ;F6     = Move
        db 147:dw cmdfol    ;F7     = Folder
        db 148:dw cmddel    ;F8     = Delete
        db 149:dw filprp    ;F9     = Properties
        db   1:dw mrkall    ;Ctrl+A = alle markieren
        db  18:dw shwref    ;Ctrl+R = Reload
        db  21:dw cmdswp    ;Ctrl+U = SwapListen
        db  32:dw lstfoc    ;Space  = Liste bekommt Focus
        db 136:dw lstfoc    ;Rauf   = Liste bekommt Focus
        db 137:dw lstfoc    ;Runter = Liste bekommt Focus
        db 138:dw inpfoc    ;Links  = Eingabe bekommt Focus
        db 139:dw inpfoc    ;Rechts = Eingabe bekommt Focus
        db   8:dw filopn5   ;Del    = ein Verzeichnis höher
        db "*":dw mrkinv    ;*      = Selektion invertieren
        db ":":dw mrkinv
        db "+":dw mrksel    ;+      = Files selektieren
        db ";":dw mrksel
        db "-":dw mrkdes    ;-      = Files deselektieren
        db  27:dw inpclr    ;Escape = Eingabe löschen
        db  13:dw inpret    ;Return = File aufrufen oder Properties ändern
        db 141:dw ctlsw1    ;Alt+F1 = Laufwerk 1 wechseln
        db 142:dw ctlsw2    ;Alt+F2 = Laufwerk 2 wechseln

prgkey  ld hl,prgkeyt
        ld b,prgkeya
        ld de,3
        ld a,(iy+4)
prgkey1 ld c,(hl)
        cp c
        jr z,prgkey2
        add hl,de
        djnz prgkey1
        cp 33
        jp c,prgprz0
        cp 128
        ld c,a
        jp c,inpfoc
        jp prgprz0
prgkey2 inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld a,7
        jp (hl)

;### PRGEND -> Programm beenden
prgend  ld ix,(prgprzn)
        db #dd:ld h,PRC_ID_SYSTEM
        ld iy,prgmsgb
        ld (iy+0),MSC_SYS_PRGEND
        ld a,(prgcodbeg+prgpstnum)
        ld (iy+1),a
        rst #10
prgend0 rst #30
        jr prgend0

;### PRGINF -> Info-Fenster anzeigen
prginf  ld hl,prgmsginf         ;*** Info-Fenster
        ld b,1+128
        call prginf0
        jp prgprz0
prginf0 ld (prgmsgb+1),hl
        ld a,(prgbnknum)
        ld c,a
        ld (prgmsgb+3),bc
        ld a,MSC_SYS_SYSWRN
        ld (prgmsgb),a
        ld ix,(prgprzn)
        db #dd:ld h,PRC_ID_SYSTEM
        ld iy,prgmsgb
        rst #10
        ret

;### PRGERR -> Disc-Error-Fenster anzeigen
;### Eingabe    CF=0 -> kein Fehler, CF=1 -> Fehler, A=Fehlercode
prgerr  ret nc
        push af
        cp 255
        ld hl,prgmsgerru
        jr z,prgerr1
        cp prgmsgerrmx+1
        ld hl,prgmsgerrs
        jr nc,prgerr1
        add a
        ld l,a
        ld h,0
        ld de,prgmsgerrtb
        add hl,de
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
prgerr1 ld (prgmsgerra),hl
        pop af
        call clcdez
        ld (prgmsgerr1a),hl
        ld b,1
        ld hl,prgmsgerr
        jp prginf0


;==============================================================================
;### SUB-ROUTINEN #############################################################
;==============================================================================

SySystem_HLPFLG db 0    ;flag, if HLP-path is valid
SySystem_HLPPTH db "%help.exe "
SySystem_HLPPTH1 ds 128
SySHInX db ".HLP",0

SySystem_HLPINI
        ld hl,(prgcodbeg)
        ld de,prgcodbeg
        dec h
        add hl,de                   ;HL = CodeEnd = Command line
        ld de,SySystem_HLPPTH1
        ld bc,0
        db #dd:ld l,128
SySHIn1 ld a,(hl)
        or a
        jr z,SySHIn3
        cp " "
        jr z,SySHIn3
        cp "."
        jr nz,SySHIn2
        ld c,e
        ld b,d
SySHIn2 ld (de),a
        inc hl
        inc de
        db #dd:dec l
        ret z
        jr SySHIn1
SySHIn3 ld a,c
        or b
        ret z
        ld e,c
        ld d,b
        ld hl,SySHInX
        ld bc,5
        ldir
        ld a,1
        ld (SySystem_HLPFLG),a
        ret

hlpopn  ld a,(SySystem_HLPFLG)
        or a
        jp z,prgprz0
        ld hl,SySystem_HLPPTH
        jp filopnb

;### MSGGET -> Message für Programm abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
;### Veraendert 
msgget  ld a,(prgprzn)
        db #dd:ld l,a           ;IXL=Rechner-Prozeß-Nummer
        db #dd:ld h,-1
        ld iy,prgmsgb           ;IY=Messagebuffer
msgget1 rst #08                 ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        or a
        db #dd:dec l
        ret nz
        ld iy,prgmsgb
        ld a,(iy+0)
        or a
        jp z,prgend
        scf
        ret

;### MSGDSK -> Message für Programm von Deskzop-Prozess abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
;### Veraendert 
msgdsk  call msgget
        jr nc,msgdsk            ;keine Message
        ld a,(dskprzn)
        db #dd:cp h
        jr nz,msgdsk            ;Message von anderem als Desktop-Prozeß -> ignorieren
        ld a,(prgmsgb)
        ret

;### MSGSND -> Message an Desktop-Prozess senden
;### Eingabe    C=Kommando, B/E/D/L/H=Parameter1/2/3/4/5
msgsnd0 ld a,(prgwin)
        ld b,a
msgsnd2 ld c,MSC_DSK_WININH
msgsnd  ld a,(dskprzn)
msgsnd1 db #dd:ld h,a
        ld a,(prgprzn)
        db #dd:ld l,a
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),b
        ld (iy+2),e
        ld (iy+3),d
        ld (iy+4),l
        ld (iy+5),h
        rst #10
        ret

;### STRADD -> Hängt String1 hinter String2
;### Eingabe    HL=String1, DE=String2
;### Verändert  BC,DE,HL
stradd  push hl
        push de
        call strlen     ;BC=Länge String1
        pop hl
        push bc
        call strlen
        ex de,hl        ;DE=Ende String2
        pop bc
        inc bc
        pop hl
        ldir
        ret

;### STRLEN -> Ermittelt Länge eines Strings
;### Eingabe    HL=String (0-terminiert)
;### Ausgabe    HL=Stringende (0), BC=Länge (maximal 255, ohne Terminator)
;### Verändert  -
strlen  push af
        xor a
        ld bc,255
        cpir
        ld a,254
        sub c
        ld c,a
        dec hl
        pop af
        ret

;### SYSCLL -> Betriebssystem-Funktion aufrufen
;### Eingabe    (SP)=Modul/Funktion, AF,BC,DE,HL,IX,IY=Register
;### Ausgabe    AF,BC,DE,HL,IX,IY=Register
sysclln db 0
syscll  ld (prgmsgb+04),bc      ;Register in Message-Buffer kopieren
        ld (prgmsgb+06),de
        ld (prgmsgb+08),hl
        ld (prgmsgb+10),ix
        ld (prgmsgb+12),iy
        push af
        pop hl
        ld (prgmsgb+02),hl
        pop hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        push hl
        ld (prgmsgb+00),de      ;Modul und Funktion in Message-Buffer kopieren
        ld a,e
        ld (sysclln),a
        ld iy,prgmsgb
        ld ix,(prgprzn)
        db #dd:ld h,PRC_ID_SYSTEM
        rst #10                 ;Message senden
syscll1 rst #30
        ld iy,prgmsgb
        ld ix,(prgprzn)
        db #dd:ld h,PRC_ID_SYSTEM
        rst #18                 ;auf Antwort warten
        db #dd:dec l
        jr nz,syscll1
        ld a,(prgmsgb)
        sub 128
        ld e,a
        ld a,(sysclln)
        cp e
        jr nz,syscll1
        ld hl,(prgmsgb+02)      ;Register aus Message-Buffer holen
        push hl
        pop af
        ld bc,(prgmsgb+04)
        ld de,(prgmsgb+06)
        ld hl,(prgmsgb+08)
        ld ix,(prgmsgb+10)
        ld iy,(prgmsgb+12)
        ret

;### DIAINP -> Öffnet Dialog-Fenster zur Eingabe von Strings
;### Eingabe    HL=Beschreibungstext, DE=Quelle InputString, A=Maxlänge InputString, BC=Adresse Routine bei "Ok"
diainp  ld (diainpdsc),hl       ;Beschreibung setzen
        ld (diainp5+1),bc       ;Ok-Call setzen
        ld (diainpinp+10),a     ;Eingabe-String vorbereiten
        ex de,hl
        ld (diainp6+1),hl
        ld b,a
        ld c,0
        ld de,diainpbuf
diainp1 ld a,(hl)
        ldi
        inc bc
        or a
        jr z,diainp2
        inc c
        djnz diainp1
        xor a
        ld (de),a
diainp2 ld (diainpinp+2),a
        ld (diainpinp+6),a
        ld a,c
        ld (diainpinp+4),a
        ld (diainpinp+8),a
        ld de,diainpwin
diainp7 ld a,1+128
        ld (de),a
        ld c,MSC_DSK_WINOPN     ;Fenster aufbauen
        ld a,(prgbnknum)
        ld b,a
        call msgsnd
diainp3 call msgdsk             ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        cp MSR_DSK_WOPNER
        ret z                   ;kein Speicher für Fenster -> dann halt nicht
        cp MSR_DSK_WOPNOK
        jr nz,diainp3           ;andere Message als "Fenster geöffnet" -> ignorieren
        ld a,(prgmsgb+4)
        ld (diawin),a           ;Fenster wurde geöffnet -> Nummer merken
        inc a
        ld (prgwindat+windatsup),a
        ret

diainpc call diainp4            ;*** CANCEL
        jp prgprz0
diainp4 call diainp9            ;Dialog-Fenster schliessen
        ld hl,256*33+50
        ld a,5
diainp8 ld (diainpgrp),a
        ld a,h
        ld (diainpdat1),a
        ld (diainpdat2),a
        ld a,l
        ld (diainpwin+10),a
        ld (diainpwin+18),a
        ld (diainpwin+26),a
        ret
diainpo call diainp4            ;*** OK
diainp6 ld de,0                 ;Eingabe-String zurückkopieren
        ld hl,diainpbuf
        ld bc,(diainpinp+10)
        ldir
diainp5 jp 0                    ;Routine aufrufen
diainp9 ld c,MSC_DSK_WINCLS     ;Dialog-Fenster schliessen
        ld a,(diawin)
        ld b,a
        jp msgsnd

;### DIAUPD -> Updated Control im Dialog-Fenster
;### Eingabe    E=Control
diaupd  ld c,MSC_DSK_WININH
        ld a,(diawin)
        ld b,a
        jp msgsnd

;### CLCUCS -> Wandelt Klein- in Großbuchstaben um
;### Eingabe    A=Zeichen
;### Ausgabe    A=ucase(Zeichen)
;### Verändert  F
clcucs  cp "a"
        ret c
        cp "z"+1
        ret nc
        add "A"-"a"
        ret

;### CLCLCS -> Wandelt Groß- in Kleinbuchstaben um
;### Eingabe    A=Zeichen
;### Ausgabe    A=lcase(Zeichen)
;### Verändert  F
clclcs  cp "A"
        ret c
        cp "Z"+1
        ret nc
        add "a"-"A"
        ret

;### CLCDEZ -> Rechnet Byte in zwei Dezimalziffern um
;### Eingabe    A=Wert
;### Ausgabe    L=10er-Ascii-Ziffer, H=1er-Ascii-Ziffer
;### Veraendert AF
clcdez  ld l,0
clcdez1 sub 10
        jr c,clcdez2
        inc l
        jr clcdez1
clcdez2 add "0"+10
        ld h,a
        ld a,"0"
        add l
        ld l,a
        ret

;### CLCN32 -> Wandelt 32Bit-Zahl in ASCII-String um (mit 0 abgeschlossen)
;### Eingabe    DE,IX=Wert, IY=Adresse
;### Ausgabe    IY=Adresse letztes Zeichen
;### Veraendert AF,BC,DE,HL,IX,IY
clcn32t dw 1,0,     10,0,     100,0,     1000,0,     10000,0
        dw #86a0,1, #4240,#f, #9680,#98, #e100,#5f5, #ca00,#3b9a
clcn32z ds 4

clcn32  ld (clcn32z),ix
        ld (clcn32z+2),de
        ld ix,clcn32t+36
        ld b,9
        ld c,0
clcn321 ld a,"0"
        or a
clcn322 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  sbc hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):sbc hl,de:ld (clcn32z+2),hl
        jr c,clcn325
        inc c
        inc a
        jr clcn322
clcn325 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  add hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):adc hl,de:ld (clcn32z+2),hl
        ld de,-4
        add ix,de
        inc c
        dec c
        jr z,clcn323
        ld (iy+0),a
        inc iy
clcn323 djnz clcn321
        ld a,(clcn32z)
        add "0"
        ld (iy+0),a
        ld (iy+1),0
        ret

;### CLCDIV -> Dividiert zwei Werte (24bit)
;### Eingabe    A,BC=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1/Wert2, DE=Wert1 MOD Wert2
;### Veraendert AF,BC,DE,IX,IYL
clcdiv  db #dd:ld l,e
        db #dd:ld h,d   ;IX=Wert2(Nenner)
        ld e,a          ;E,BC=Wert1(Zaehler)
        ld hl,0
        db #dd:ld a,l
        db #dd:or h
        ret z
        ld d,l          ;D,HL=RechenVar
        db #fd:ld l,24  ;IYL=Counter
clcdiv1 rl c
        rl b
        rl e
        rl l
        rl h
        rl d
        ld a,l
        db #dd:sub l
        ld l,a
        ld a,h
        db #dd:sbc h
        ld h,a
        ld a,d
        sbc 0
        ld d,a          ;D,HL=D,HL-IX
        jr nc,clcdiv2
        ld a,l
        db #dd:add l
        ld l,a
        ld a,h
        db #dd:adc h
        ld h,a
        ld a,d
        adc 0
        ld d,a
        scf
clcdiv2 ccf
        db #fd:dec l
        jr nz,clcdiv1
        ex de,hl        ;DE=Wert1 MOD Wert2
        rl c
        rl b
        ld l,c
        ld h,b          ;HL=Wert1 DIV Wert2
        ret

;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### Eingabe    A=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1*Wert2 (16bit)
;### Veraendert AF,DE
clcm16  ld hl,0
        or a
clcm161 rra
        jr nc,clcm162
        add hl,de
clcm162 sla e
        rl d
        or a
        jr nz,clcm161
        ret

;### CLCR16 -> Wandelt String in 16Bit Zahl um
;### Eingabe    IX=String, A=Terminator, BC=Untergrenze (>=0), DE=Obergrenze (<=65534)
;### Ausgabe    IX=String hinter Terminator, HL=Zahl, CF=1 -> Ungültiges Format (zu groß/klein, falsches Zeichen/Terminator)
;### Veraendert AF,DE,IYL
clcr16  ld hl,0
        db #fd:ld l,a
clcr161 ld a,(ix+0)
        inc ix
        db #fd:cp l
        jr z,clcr163
        sub "0"
        cp 10
        ccf
        ret c
        push bc
        add hl,hl:jr c,clcr162
        ld c,l
        ld b,h
        add hl,hl:jr c,clcr162
        add hl,hl:jr c,clcr162
        add hl,bc:jr c,clcr162
        ld c,a
        ld b,0
        add hl,bc:ret c
        pop bc
        jr clcr161
clcr162 pop bc
        ret
clcr163 sbc hl,bc
        ret c
        add hl,bc
        inc de
        sbc hl,de
        ccf
        ret c
        add hl,de
        or a
        ret


;==============================================================================
;### CONTROL FUNKTIONEN #######################################################
;==============================================================================

;### CTLSW1/2 -> Rechtes/Linkes Laufwerk erhöhen
ctlsw1  ld ix,prgobjdeva
        ld c,0
        jr ctlswt
ctlsw2  ld ix,prgobjdevb
        ld c,1
ctlswt  push ix
        push bc
        ld hl,jmp_keysta
        rst #28
        pop bc
        pop ix
        bit 2,e
        jp z,prgprz0
        ld a,(ix+12)
        inc a
        cp (ix+0)
        jr nz,ctlswt1
        xor a
ctlswt1 ld (ix+12),a
        push bc
        ld e,c
        inc e
        call msgsnd0
        pop bc
        dec c
        jr z,ctldr2
        jr ctldr1

;### CTLDR1/2 -> Rechtes/Linkes Laufwerk wechseln
ctldr1  ld ix,pthlen1
        ld iy,lstlay1
        ld a,(prgobjdeva+12)
        ld e,0
        jr ctldrv
ctldr2  ld ix,pthlen2
        ld iy,lstlay2
        ld a,(prgobjdevb+12)
        ld e,1
ctldrv  ld b,a
        add a
        add a
        add b
        ld l,a
        ld h,0
        ld bc,prgtabdev+2
        add hl,bc
        ld a,(hl)
        cp (ix+1)
        jp z,prgprz0
        ld (iy+0),0
        ld (ix+0),1
        ld (ix+1),a
        ld (ix+4),0
        ld a,e
        push af
        call pthupd
        pop af
        push af
        call lstref
        pop af
        or a
        jp z,lstfoca
        jp lstfocb


;==============================================================================
;### "FILE" FUNKTIONEN ########################################################
;==============================================================================

;### FILOPN -> Öffnet File (Verzeichnis wechseln, Programm starten oder Dokument laden)
filopn  scf
        call lstful         ;ZF=0 -> Eintrag gefunden, (cmdpth)=voller Pfad, (diainpbuf)=Filename [0=Länge, 1-x=Name]
        jp z,prgprz0        ;keine Datei ausgewählt -> Abbruch
filopn0 ld a,(lstentt)
        or a
        jp z,filopn1
        ld ix,diainpbuf
        ld a,(ix+1)             ;*** Verzeichnis wechseln
        cp "."
        jr z,filopn5
        ld a,(lstakt)           ;** Ebene tiefer
        or a
        ld hl,pthlen1
        ld iy,lstlay1
        jr z,filopn2
        ld hl,pthlen2
        ld iy,lstlay2
filopn2 ld a,(ix+0)
        or a
        jp z,prgprz0
        ld c,(iy+0)
        inc (iy+0)
        ld b,0
        add iy,bc
        add iy,bc
        ld bc,(lstentp)
        ld (iy+1),c
        ld (iy+2),b
        push hl
        ld c,(hl)
        inc c:inc c:inc c
        ld b,0
        add hl,bc
        ex de,hl
        ld hl,diainpbuf+1
        ld b,a
filopn3 ld a,(hl)
        inc hl
        call clclcs
        ld (de),a
        inc de
        djnz filopn3
        cp "."
        ld a,(ix+0)
        jr nz,filopn6
        dec a
        dec de
filopn6 ex de,hl
        ld (hl),"\"
        inc hl
        ld (hl),0
        pop hl
        inc a
        add (hl)
        ld (hl),a
filopn4 ld a,(lstakt)
        push af
        call pthupd
        pop af
        call lstref
        jp prgprz0
filopn5 ld a,(lstakt)           ;** Ebene höher
        or a
        ld hl,pthlen1
        ld iy,lstlay1
        jr z,filopn7
        ld hl,pthlen2
        ld iy,lstlay2
filopn7 ld a,(hl)
        cp 1
        jp z,prgprz0
        add 2
        ld c,a
        ld b,0
        ld a,(iy+0)
        sub 1
        jr c,filopna
        ld (iy+0),a
        add a
        ld e,a
        ld d,0
        add iy,de
        ld e,(iy+1)
        ld d,(iy+2)
        ld (lstrefp),de
filopna ld e,l
        ld d,h                  ;DE=Len
        add hl,bc               ;HL=letztes Zeichen
filopn8 inc b
        dec hl
        ld a,(hl)
        cp "/"
        jr z,filopn9
        cp "\"
        jr z,filopn9
        jr filopn8
filopn9 inc hl
        ld (hl),0
        ex de,hl
        ld a,(hl)
        sub b
        ld (hl),a
        jr filopn4
filopn1 ld hl,cmdpth
filopnb ld c,MSC_SYS_PRGRUN     ;*** Datei öffnen
        ld a,(prgbnknum)
        ld d,a
        ld b,l
        ld e,h
        ld a,PRC_ID_SYSTEM
        call msgsnd1
        jp prgprz0

;### FILATR -> Erlaubt das Ändern von Attributen und Timestamps mehrerer Files gleichzeitig
filatrm db 0
filatrt db 0
filatry dw 0
filatrh db 0

filatr  call lstanz
        jp z,prgprz0                ;keine Datei ausgewählt -> Abbruch
        xor a
        ld (attribtim),a
        ld a,2
        ld (attribrad1),a
        ld (attribrad2),a
        ld (attribrad3),a
        ld (attribrad4),a
        call filatr1
        ld de,attribwin
        call diainp7
        jp prgprz0

filatr1 ld a,10                     ;*** Aktuelle Uhrzeit eintragen
        ld ix,attribinp1
        call filatr2
        ld a,5
        ld ix,attribinp2
        call filatr2
        rst #20:dw jmp_timget       ;A=Sekunden, B=Minuten, C=Stunden, D=Tag (ab 1), E=Monat (ab 1), HL=Jahr, IXL=Timezone (-12 bis +13)
        push hl
        ld a,c
        call clcdez
        ld (attribbuf2+0),hl
        ld a,b
        call clcdez
        ld (attribbuf2+3),hl
        ld a,d
        call clcdez
        ld (attribbuf1+0),hl
        ld a,e
        call clcdez
        ld (attribbuf1+3),hl
        pop ix
        ld de,0
        ld iy,attribbuf1+6
        call clcn32
        ld a,"."
        ld (attribbuf1+2),a
        ld (attribbuf1+5),a
        ld a,":"
        ld (attribbuf2+2),a
        ret
filatr2 ld (ix+4),a
        ld (ix+8),a
        xor a
        ld (ix+2),a
        ld (ix+6),a
        ret

filatr3 call filatr1                ;*** Button "Current" wurde geklickt
        ld e,22
        call diaupd
        ld e,24
        call diaupd
        jp prgprz0

filatr0 call diainp4                ;*** Attribute und Timestamp setzen
        ld a,(attribtim)
        or a
        jp z,filatrf
        ld ix,attribbuf1            ;Timestamp vorbereiten
        ld bc,1   :ld de,31  :ld a,".":call clcr16:jp c,filatre:ld a,l:ld (filatrt),a   ;Tag
        ld bc,1   :ld de,12  :ld a,".":call clcr16:jr c,filatre:ld a,l:ld (filatrm),a   ;Monat
        ld bc,1980:ld de,2107:xor a   :call clcr16:jr c,filatre:       ld (filatry),hl  ;Jahr
        ld ix,attribbuf2
        ld bc,0   :ld de,23  :ld a,":":call clcr16:jr c,filatre:ld a,l:ld (filatrh),a   ;Stunde
        ld bc,0   :ld de,59  :xor a   :call clcr16:jr c,filatre                         ;Minute
        ld b,l
        ld a,(filatrh)
        ld c,a
        xor a
        ld hl,(filatry)
        ld de,(filatrm)

        sla b:sla b
        sla b:rl c
        sla b:rl c
        sla b:rl c
        srl a
        or b
        db #dd:ld l,a
        db #dd:ld h,c       ;IX=Uhrzeit
        ld bc,-1980
        add hl,bc
        sla e:sla e:sla e:sla e
        sla e:rl l
        ld a,d
        or e
        ld h,l
        ld l,a              ;HL=Datum

        ld (filatrm),ix
        ld (filatry),hl
        jr filatrf
filatre xor a
        ld (attribtim),a
filatrf ld ix,attribrad1
        ld a,1
        ld d,0                      ;Setzen-Maske  (Bits, die gesetzt werden sollen)
        cp (ix+0):jr nz,filatr4:set 0,d
filatr4 cp (ix+1):jr nz,filatr5:set 1,d
filatr5 cp (ix+2):jr nz,filatr6:set 2,d
filatr6 cp (ix+3):jr nz,filatr7:set 5,d
filatr7 inc a
        ld e,-1                     ;Neutral-Maske (Bits, die nicht verändert werden sollen [complement])
        cp (ix+0):jr z,filatr8:res 0,e
filatr8 cp (ix+1):jr z,filatr9:res 1,e
filatr9 cp (ix+2):jr z,filatra:res 2,e
filatra cp (ix+3):jr z,filatrb:res 5,e
filatrb ld a,e
        cpl
        and d
        ld d,a
        scf
        jr filatrd
filatrc or a
filatrd push de                     ;*** Pfad holen
        call lstful
        pop de
        jp z,shwref1
        ld hl,cmdpth                ;*** Attribut setzen
        ld a,(prgbnknum)
        db #dd:ld h,a
        xor a
        push de
        push hl
        push ix
        call syscll                 ;C=0=ReadOnly, 1=Hidden, 2=System, 3=VolumeID, 4=Directory, 5=Archive, #0f=Longname
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRPRR           ;Datei-Eigenschaften lesen -> Attribute
        pop ix
        pop hl
        pop de
        jp c,filprp7
        ld a,c
        and e
        or d
        ld c,a
        xor a
        push de
        push hl
        push ix
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRPRS           ;Datei-Eigenschaften setzen -> Attribute
        pop ix
        pop hl
        pop de
        jp c,filprp7
        ld a,(attribtim)            ;*** Timestamp setzen
        or a
        jr z,filatrc
        push de
        push hl
        push ix
        ld bc,(filatrm)
        ld de,(filatry)
        ld a,1
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRPRS           ;Datei-Eigenschaften setzen -> Timestamp
        pop ix
        pop hl
        pop de
        jp c,filprp7
        jr filatrc

;### FILDRV -> Zeigt Laufwerks-Informationen an
fildrvs ds 4
fildrv  ld a,(lstakt)
        or a
        ld a,(pthdev1)
        jr z,fildrv1
        ld a,(pthdev2)
fildrv1 call clcucs
        ld (drvprptxt0+6),a
        ld c,0
        call syscll                 ;A=Typ, B=Medium, C=Filesystem, D=Sektoren pro Cluster, IY,IX=Anzahl Cluster
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRINF
        jp c,filprp7
        ld a,b                      ;Medium
        and 127
        cp 01:ld hl,drvprptxt2a:jr z,fildrv2
        cp 02:ld hl,drvprptxt2b:jr z,fildrv2
        cp 16:ld hl,drvprptxt2c:jr z,fildrv2
        cp 17:ld hl,drvprptxt2d:jr z,fildrv2
        cp 18:ld hl,drvprptxt2e:jr z,fildrv2
        ld hl,drvprptxt20
fildrv2 ld (drvprpdsc2),hl
        ld a,c                      ;Filesystem
        cp 01:ld hl,drvprptxt4a:jr z,fildrv3
        cp 02:ld hl,drvprptxt4b:jr z,fildrv3
        cp 03:ld hl,drvprptxt4c:jr z,fildrv3
        cp 16:ld hl,drvprptxt4d:jr z,fildrv3
        cp 17:ld hl,drvprptxt4e:jr z,fildrv3
        cp 18:ld hl,drvprptxt4f:jr z,fildrv3
        ld hl,drvprptxt20
fildrv3 ld (drvprpdsc4),hl
        rl b                        ;Wechseldatenträger
        ld hl,drvprptxtb0
        jr nc,fildrv7
        ld hl,drvprptxtb1
fildrv7 ld (drvprpdscb),hl
        ;[...sektoren pro cluster...]
fildrv8 srl d                       ;Gesamt-Kapazität
        jr z,fildrv9
        add ix,ix
        push af
        add iy,iy
        pop af
        jr nc,fildrv8
        inc iy
        jr fildrv8
fildrv9 push iy:pop de
        call fildrv6
        ld (fildrvs+0),ix
        ld (fildrvs+2),de
        ld iy,drvprptxta
        ld hl,drvprptxt62
        call filprx9                ;IY=Ziel, HL=Anhang, DE,IX=Zahl
        ld hl,drvprptxt60
        ld (drvprpdsc6),hl          ;Belegt/Frei bisher unbekannt
        ld (drvprpdsc8),hl
        ld de,drvprpwin
        call diainp7                ;Fenster aufbauen
        ld a,(drvprptxt0+6)
        ld c,1
        call syscll                 ;IY,IX=Gesamtanzahl Cluster, HL,DE=Anzahl freier 512B-Sektoren
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRINF
        jr c,fildrv4
        push de:pop ix
        ex de,hl
        call fildrv6
        push de
        push ix
        ld iy,drvprptxt8            ;"Noch frei" eintragen
        ld hl,drvprptxt62
        call filprx9                ;IY=Ziel, HL=Anhang, DE,IX=Zahl
        or a
        ld hl,(fildrvs+0)
        pop bc
        sbc hl,bc
        push hl:pop ix
        ld hl,(fildrvs+2)
        pop bc
        sbc hl,bc
        ex de,hl
        ld iy,drvprptxt6            ;"Belegt" eintragen
        ld hl,drvprptxt62
        call filprx9                ;IY=Ziel, HL=Anhang, DE,IX=Zahl
        ld hl,drvprptxt6
        ld (drvprpdsc6),hl
        ld hl,drvprptxt8
        jr fildrv5
fildrv4 ld hl,drvprptxt61           ;Fehler beim Free-Untersuchen
        ld (drvprpdsc6),hl
fildrv5 ld (drvprpdsc8),hl
        ld de,256*11+256-2
        ld a,(diawin)
        ld b,a
        call msgsnd2
        jp prgprz0
fildrv6 srl d:rr e
        db #dd:ld a,h:rr a:db #dd:ld h,a
        db #dd:ld a,l:rr a:db #dd:ld l,a
        ret

;### FILPRP -> Zeigt File-Eigenschaften an und erlaubt das Umbenennen und das Ändern von Attributen
filprpx ds 768          ;Extension-Daten
propertxt8a db "[not defined]",0
propertxt8b db "[Executable]",0
propertxt8c db "[Directory]",0

filprpa db 0            ;altes Attribut des angezeigten Files

filprp  ld e,7                  ;*** File Extensions holen
        ld hl,jmp_sysinf
        rst #28             ;DE=System, IX=Data, IY=Transfer
        push ix
        pop hl
        ld bc,400+896+192+1176
        add hl,bc
        ld de,filprpx
        ld bc,768
        ld a,(prgbnknum)
        add a:add a:add a:add a
        inc a
        rst #20:dw jmp_bnkcop
        scf
        ld a,1
        call lstnxt             ;*** erstes selektiertes File holen
        jp z,prgprz0
        ld a,(lstakt)           ;kompletten Pfad vorbereiten
        or a
        ld hl,pthdev1
        jr z,filprp1
        ld hl,pthdev2
filprp1 ld (propercon3),hl
        ld de,cmdpth
        push de
        ld bc,256
        ldir
        pop hl
        call strlen
        ex de,hl
        ld hl,diainpbuf+1
        ld a,(diainpbuf)
        ld c,a
        ld (diainpinp+8),a
        ld a,255
        ld (diainpinp+10),a
        xor a
        ld (diainpinp+2),a
        ld (diainpinp+4),a
        ld (diainpinp+6),a
        ld b,a
        ld a,c
        inc bc
        ldir
filprp2 ld b,a                      ;*** Extension
        ld a,32
        ld (propertxte+0),a
        ld (propertxte+1),a
        ld (propertxte+2),a
        ld a,"."
        dec hl
filprp3 dec hl
        cp (hl)
        jr z,filprp4
        inc c
        djnz filprp3
        jr filprp5
filprp4 inc hl
        inc c
        dec c
        jr z,filprp5
        ld de,propertxte
filprp6 ld a,(hl)
        call clcucs
        ld (de),a
        inc hl
        inc de
        dec c
        jr nz,filprp6
filprp5 ld a,(lstentt)              ;*** Typ (Starten mit)
        or a
        ld hl,propertxt8c
        jr nz,filprx5
        ld a,(propertxte+0)
        call clclcs
        ld c,a
        ld hl,(propertxte+1)
        ld a,l
        call clclcs
        ld e,a
        ld a,h
        call clclcs
        ld d,a
        ld a,"e"
        cp d
        jr nz,filprx6
        cp c
        jr nz,filprx6
        ld a,"x"
        cp e
        ld hl,propertxt8b
        jr z,filprx5
filprx6 ld hl,filprpx
        ld b,16
filprx1 push bc
        push hl
        ld b,4
filprx2 push hl
               ld a,(hl):cp c:jr nz,filprx3
        inc hl:ld a,(hl):cp e:jr nz,filprx3
        inc hl:ld a,(hl):cp d
filprx3 pop hl
        jr z,filprx4
        inc hl:inc hl:inc hl
        djnz filprx2
        pop hl
        ld bc,48
        add hl,bc
        pop bc
        djnz filprx1
        ld hl,propertxt8a
        jr filprx5
filprx4 pop hl
        pop bc
        ld bc,15
        add hl,bc
filprx5 ld de,propertxt8
        ld bc,32
        ldir

        ld a,(diainpbuf+1)
        ld hl,(diainpbuf+2)
        push af
        push hl
        ld a,(lstnxtb)              ;*** Länge
        ld de,(lstnxtz)
        ld hl,(lstnxtp)
        dec hl
        ld c,3
        call lstent
        ld iy,propertxtf
        ld hl,propertxtg
        ld ix,(diainpbuf+0)
        ld de,(diainpbuf+2)
        call filprx9
        pop hl
        pop af
        ld (diainpbuf+1),a
        ld (diainpbuf+2),hl
        ld hl,diainpbuf+1
        ld de,diainpbuf
        ld bc,255
        ldir
        ld hl,cmdpth                ;*** Attribute
        ld a,(prgbnknum)
        db #dd:ld h,a
        xor a
        call syscll                 ;C=0=ReadOnly, 1=Hidden, 2=System, 3=VolumeID, 4=Directory, 5=Archive, #0f=Longname
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRPRR           ;Datei-Eigenschaften lesen -> Attributes
        jr c,filprp7
        ld a,c
        ld (filprpa),a
        xor a
        ld b,a:rr c:adc b:ld (properatr1),a
        ld a,b:rr c:adc b:ld (properatr2),a
        ld a,b:rr c:adc b:ld (properatr3),a
        rr c
        rr c
        ld hl,propertxte
        jr nc,filprp8
        ld hl,propertxth
filprp8 ld (propercon1),hl
        ld a,b:rr c:adc b:ld (properatr4),a
        ld hl,cmdpth                ;*** Timestamp Created
        ld a,(prgbnknum)
        db #dd:ld h,a
        ld a,2
        call syscll                 ;DE,BC=Timestamp
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRPRR           ;Datei-Eigenschaften lesen -> Date Created
        jr c,filprp7
        call syscll                 ;A=Sekunden, B=Minuten, C=Stunden, D=Tag (ab 1), E=Monat (ab 1), HL=Jahr
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILF2T
        ld iy,properdtm1+6
        call filprp9
        ld hl,cmdpth                ;*** Timestamp Modified
        ld a,(prgbnknum)
        db #dd:ld h,a
        ld a,1
        call syscll                 ;DE,BC=Timestamp
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRPRR           ;Datei-Eigenschaften lesen -> Date Created
        jr c,filprp7
        call syscll                 ;A=Sekunden, B=Minuten, C=Stunden, D=Tag (ab 1), E=Monat (ab 1), HL=Jahr
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILF2T
        ld iy,properdtm2+6
        call filprp9
        ld de,properwin
        call diainp7
        jp prgprz0
filprp7 call prgerr                 ;file error code
        jp prgprz0
filprp9 push hl                     ;*** Timestamp ab iy+6 eintragen
               call clcdez:ld (iy+18-6),l:ld (iy+19-6),h    ;Sekunde
        ld a,b:call clcdez:ld (iy+15-6),l:ld (iy+16-6),h    ;Minute
        ld a,c:call clcdez:ld (iy+12-6),l:ld (iy+13-6),h    ;Stunde
        ld a,d:call clcdez:ld (iy+00-6),l:ld (iy+01-6),h    ;Tag
        ld a,e:call clcdez:ld (iy+03-6),l:ld (iy+04-6),h    ;Monat
        pop ix
        ld de,0
        call clcn32                                         ;Jahr
        ld (iy+1),","
        ret

filprx9 push hl         ;IY=Ziel, HL=Anhang, DE,IX=Zahl
        call clcn32
        db #fd:ld e,l
        db #fd:ld d,h
        inc de
        pop hl
        ld bc,8
        ldir
        ret

filprp0 call diainp4                ;*** File umbenennen und Attribute setzen
        ld ix,properatr1
        xor a
        ld c,a
        cp (ix+0)
        jr z,filprpb
        set 0,c
filprpb cp (ix+1)
        jr z,filprpc
        set 1,c
filprpc cp (ix+2)
        jr z,filprpd
        set 2,c
filprpd cp (ix+3)
        jr z,filprpe
        set 5,c
filprpe ld a,(filprpa)
        and #ff-#27
        or c
        ld c,a
        ld a,(prgbnknum)
        db #dd:ld h,a
        ld hl,cmdpth
        xor a
        push hl
        push ix
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRPRS           ;Datei-Eigenschaften setzen -> Attribute
        pop ix
        pop hl
        jp c,filprp7
        ld de,diainpbuf
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRREN           ;Datei/Verzeichnis umbenennen
        jp shwref1

;### FILSPL -> Splittet File in mehrere Sub-Files auf
filspls dw 0                ;Größe der Split-File in KB
filsplg db 0                ;Größe des Copy-Buffers in KB
filspll dw 0                ;noch zu kopierende KBs in einzelnes Ziel-File
filsplo db 0                ;Flag, ob Ziel offen

filspl  ld hl,256*47+64
        ld a,7
        call diainp8
        ld iy,cmdcoptxt1
        ld hl,splitttxt1
        ld bc,filspl0
        jp cmdcop0
filspl0 xor a                   ;*** Operation ausführen
        ld (filsplo),a
        ld ix,splittbuf
        xor a
        ld bc,1
        ld de,65534
        call clcr16
        jp c,prgprz0            ;Error -> falsche Größenangabe vom Ziel
        ld (filspls),hl         ;Größe eintragen
        ld a,(cfgbuf+1)
        srl a:srl a             ;A=Buffer-Größe in KB
        ld (filsplg),a
        scf
        call lstful             ;ZF=0 -> Eintrag gefunden, (cmdpth)=voller Pfad, (diainpbuf)=Filename [0=Länge, 1-x=Name]
        jp z,prgprz0
        call prgcop0
        jp c,prgprz0            ;Error -> Speicher voll
        ld a,64
        ld (copmovdat1-2),a
        ld hl,copmovtxt0
        ld (copmovsrc),hl
        ld (copmovdst),hl
        ld de,copmovwin
        call diainp7            ;Fenster öffnen
        call filsplf
        ld (cmdpthz),hl
        inc hl
        inc hl
        inc hl
        xor a
        ld (hl),a
        ld l,a
        ld h,a
        ld (cmdcnt),hl

        ld hl,cmdpth                ;*** Quelle öffnen
        ld (copmovsrc),hl
        ld e,4
        push hl
        call diaupd                 ;Pfad anzeigen
        pop hl
        ld a,(prgbnknum)
        db #dd:ld h,a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOPN
        jp c,filspla
        ld (prgcops),a

filspl3 ld hl,(cmdcnt)              ;### Split-Loop
        inc hl
        ld (cmdcnt),hl
        dec hl
        ld bc,100
        ld a,"0"-1
filsplh inc a
        or a
        sbc hl,bc
        jr nc,filsplh
        ld ix,(cmdpthz)
        ld (ix+0),a
        ld a,l
        add c
        call clcdez
        ld (ix+1),l
        ld (ix+2),h
        ld hl,(filspls)
        ld (filspll),hl             ;HL=KBs, die in einzelnes Ziel-File müssen

filspl4 ld hl,(filspll)             ;*** zu kopierende Größe errechnen
        ld a,l
        or h
        jr nz,filspl7
        call prgcop4                ;Ziel schließen
        xor a
        ld (filsplo),a
        jr filspl3
filspl7 ld de,(filsplg)
        ld a,l
        ld c,0
        ld d,c
        or a
        sbc hl,de                   ;HL = noch zu kopieren - buffer größe
        jr nc,filspl5
        ld hl,0
        jr filspl6
filspl5 ld a,e
filspl6 ld (filspll),hl
        add a:add a
        ld b,a                      ;BC=Buffer-Größe

        ld a,(prgcopb)              ;*** Source lesen
        ld e,a
        ld hl,(prgcopa)
        ld a,(prgcops)
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        jr c,filsple
        ld a,c
        or b
        jr z,filspl8                ;Source zuende -> fertig

        ld a,(filsplo)
        or a
        jr nz,filspl9
        push bc                     ;*** Ziel öffnen
        ld hl,cmdpth2
        ld (copmovdst),hl
        ld e,5
        push hl
        call diaupd                 ;Pfad anzeigen
        pop hl
        ld a,(prgbnknum)
        db #dd:ld h,a
        xor a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILNEW
        jp c,filsplb
        ld (prgcopd),a
        ld a,1
        ld (filsplo),a
        pop bc

filspl9 ld a,(prgcopb)              ;*** Ziel schreiben
        ld e,a
        ld hl,(prgcopa)
        ld a,(prgcopd)
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOUT
        jr c,filsplc
        dec a
        ld a,24
        scf
        jr z,filsplc
        jp filspl4

filspl8 call prgcop3
        call prgcop4
        jp cmdcop5

filspla push af                 ;*** Fehler -> Memory freigeben
filspld call diainp4
        pop af
        call prgerr
        jp cmdcop6
filsplb push af                 ;*** Fehler -> Memory + Quell freigeben
        call prgcop3
        jr filspld
filsplc push af                 ;*** Fehler -> Memory + Quell + Ziel freigeben
        call prgcop3
        call prgcop4
        jr filspld
filsple ld a,(filsplo)          ;*** Fehler -> Memory + Quell (+ Ziel) freigeben
        or a
        jr z,filsplb
        jr filsplc

filsplf ld hl,cmdpth2           ;erstes File an Zielpfad ohne Extension hängen -> (HL)=Ende
        call strlen
        ex de,hl
        ld hl,diainpbuf+1
filspl1 ld a,(hl)
        or a
        jr z,filspl2
        cp "."
        jr z,filspl2
        ldi
        jr filspl1
filspl2 ex de,hl
        ld (hl),"."
        inc hl
        ret

;### FILCMB -> Fügt mehrere Files zu einem zusammen
filcmb  ld iy,cmdcoptxt1
        call cmdcop8
        jp z,prgprz0
        ld a,1
        scf
        call lstnxt
        jp z,prgprz0
        ld a,2
        ld (cmdcopm),a
        call filsplf
        ld (hl),"c"
        inc hl
        ld (hl),"m"
        inc hl
        ld (hl),"b"
        inc hl
        ld (hl),0
        ld hl,combintxt
        ld bc,cmdcop1
        jp cmdcop9


;==============================================================================
;### "MARK" FUNKTIONEN ########################################################
;==============================================================================

mrkmsk  db "*.*":ds 16+1-3

;### MRKDES -> Deselektiert Files mit bestimmter Maske
mrkdes  ld a,1
        ld (mrkselm),a
        jr mrksel0

;### MRKSEL -> Selektiert Files mit bestimmter Maske
mrkselm db 0                    ;0=selektieren, 1=deselektieren

mrksel  xor a
        ld (mrkselm),a
mrksel0 ld bc,mrksel1           ;Dialog-Fenster aufbauen
        ld de,mrkmsk
        ld a,16
mrksel2 ld hl,filtyptxt
        call diainp
        jp prgprz0
mrksel1 scf                     ;*** Operation ausführen
        jr mrksel4
mrksel3 or a
mrksel4 ld a,(mrkselm)
        call lstnxt             ;ZF=0 -> Eintrag gefunden, HL=Pos, (diainpbuf)=Filename [0=Länge, 1-x=Name], DE=Zeilendaten, (lstnxtb)=Bank
        jr z,mrksel5
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,de

        push hl
        ld de,mrkmsk            ;*** Filename auf Gültigkeit anhand Maske prüfen
        ld hl,diainpbuf+1
mrkselb ld a,(de)
        inc de
        or a                    ;Ende -> auch File muß zuende sein
        jr nz,mrkselc
        cp (hl)
        jr mrksel6
mrkselc cp "?"                  ;? -> Filebuchstabe egal, falls Abschnittsende Zeiger nicht erhöhen
        jr nz,mrksel7
        call mrksel9
        jr z,mrkselb
        inc hl
        jr mrkselb
mrksel7 cp "*"                  ;* -> Filebuchstaben bis Abschnittsende egal
        jr nz,mrksela
mrksel8 call mrksel9
        jr z,mrkselb
        inc hl
        jr mrksel8
mrksela call clcucs             ;Sonstiges -> Zeichen müssen gleich sein
        ld c,a
        ld a,(hl)
        inc hl
        call clcucs
        cp c
        jr z,mrkselb
mrksel6 pop hl

        jr nz,mrksel3
        inc hl
        ld a,(lstnxtb)
        rst #20:dw jmp_bnkrbt   ;Markierung lesen
        dec hl
        push af
        ld a,(mrkselm)
        xor 1
        rrca
        ld c,a
        ld a,b                  ;Markierung ändern
        and 127
        or c
        ld b,a
        pop af
        rst #20:dw jmp_bnkwbt   ;Markierung schreiben
        jr mrksel3
mrksel5 ld a,(lstakt)           ;Liste neu anzeigen
        add 5
        db #dd:ld h,a
        jr mrkall2
;(HL)=Zeichen -> A=Zeichen, ZF=1 Abschnittsende
mrksel9 ld a,(hl)
        or a
        ret z
        cp "."
        ret

;### MRKALL -> Selektiert alle Files
mrkall  call lstget     ;A=Bank, DE=Kopf, HL=Zeilen-Daten, BC=Anzahl Zeilen, IXH=Control, ZF=1 -> keine Zeilen
        jp z,prgprz0
        ld e,c
        ld d,b
mrkall1 call mrkall5
        jr z,mrkall3
        rst #20:dw jmp_bnkrwd
        dec hl
        dec hl
        res 6,b
        set 7,b
        rst #20:dw jmp_bnkwwd
        ld bc,8-2
mrkall3 add hl,bc
        dec de
        ld c,a
        ld a,e
        or d
        ld a,c
        jr nz,mrkall1
;IXH=Control-Nummer
mrkall2 push ix
        ld a,(lstakt)
        or a
        call lstinf             ;File/Size-Info refreshen
        pop de
        ld e,d
        ld c,MSC_DSK_WININH
        ld a,(prgwin)
        ld b,a
        call msgsnd
        jp prgprz0
mrkall5 push af
        push hl
        inc hl
        inc hl
        rst #20:dw jmp_bnkrwd
        ld l,c
        ld h,b
        rst #20:dw jmp_bnkrwd
        ld c,a
        ld a,"["
        cp b:jr nz,mrkall4
        ld a,c
        rst #20:dw jmp_bnkrwd
        ld a,"."
        cp c:jr nz,mrkall4
        cp b
mrkall4 pop hl
        pop bc
        ld a,b
        ld bc,8
        ret

;### MRKNON -> Deselektiert alle Files
mrknon  call lstget
        jp z,prgprz0
        ld e,c
        ld d,b
mrknon1 call mrkall5
        jr z,mrknon2
        rst #20:dw jmp_bnkrwd
        dec hl
        dec hl
        res 6,b
        res 7,b
        rst #20:dw jmp_bnkwwd
        ld bc,8-2
mrknon2 add hl,bc
        dec de
        ld c,a
        ld a,e
        or d
        ld a,c
        jr nz,mrknon1
        jr mrkall2

;### MRKINV -> Invertiert Selektierung
mrkinv  call lstget
        jp z,prgprz0
        ld e,c
        ld d,b
        db #dd:ld l,a
mrkinv1 call mrkall5
        jr z,mrkinv2
        rst #20:dw jmp_bnkrwd
        dec hl
        dec hl
        ld a,b
        res 6,a
        xor 128
        ld b,a
        db #dd:ld a,l
        rst #20:dw jmp_bnkwwd
        ld bc,8-2
mrkinv2 add hl,bc
        dec de
        ld a,e
        or d
        db #dd:ld a,l
        jr nz,mrkinv1
        jp mrkall2

;### MRKCMP -> Vergleicht Verzeichnisse und markiert jeweils die Files, die im anderen Verzeichnis nicht vorhanden sind
mrkcmp  ;...
        jp prgprz0


;==============================================================================
;### "COMMAND" FUNKTIONEN #####################################################
;==============================================================================

cmdpth  ds 256
cmdpth2 ds 256
cmdpthz dw 0        ;Zeiger auf Ende von Zielpfad
cmdanz  dw 0
cmdcnt  dw 0

cfgovrtmp   db 0    ;0=immer überschreiben, 1=fragen, 2=immer skippen

cmdcnc  jp prgprz0

;### CMDCOP -> Kopiert ein oder mehrere Files
cmdcopm db 0        ;0=kopieren, 2=aneinanderhängen (1=Zwischenstatus -> Aneinanderhängen, Ziel bereits offen)
cmdcopt db 0        ;0=kopieren, 1=bewegen
cmdcop  ld iy,cmdcoptxt1
        ld bc,cmdcop1
        ld hl,cmdcoptxt
cmdcop0 call cmdcop8
        jp z,prgprz0
cmdcop9 ld de,cmdpth2           ;Dialog-Fenster aufbauen
        ld a,255
        call diainp
        jp prgprz0
cmdcop8 push bc                 ;*** Copy-Fenster vorbereiten
        push hl
        push iy
        call lstanz
        pop iy
        pop hl
        pop bc
        ret z                   ;keine Datei ausgewählt -> Abbruch
        ld (cmdanz),de
        push bc
        push hl
        push de
        ld a,(lstakt)           ;Pfad vorbereiten
        or a
        ld hl,pthdev2
        jr z,cmdcop4
        ld hl,pthdev1
cmdcop4 ld de,cmdpth2
        ld bc,256
        ldir
        pop ix                  ;Beschreibung vorbereiten
        ld de,0
        call clcn32
        db #fd:ld e,l
        db #fd:ld d,h
        inc de
        ld hl,cmdcoptxt2
        ld bc,12
        ldir
        pop hl
        pop bc
        ld a,1
        or a
        ret

cmdcop1 xor a
        ld (cmdcopt),a
cmdcopz ld a,(cfgovr)
        ld (cfgovrtmp),a
        call prgcop0            ;*** Operation ausführen
        jp c,prgprz0            ;Error -> Speicher voll
        ld a,4
        ld (copmovdat1-2),a
        xor a
        ld (copmovdat1+1),a
        ld l,a
        ld h,a
        ld (cmdcnt),hl
        ld hl,copmovtxt1a
        ld (copmovdsc1),hl
        ld hl,copmovtxt0
        ld (copmovsrc),hl
        ld (copmovdst),hl
        ld hl,cmdpth2
        call strlen
        ld (cmdpthz),hl
        ld de,copmovwin
        call diainp7            ;Fenster öffnen
        ld hl,cmdpth
        ld (copmovsrc),hl
        ld hl,cmdpth2
        ld (copmovdst),hl
        scf
        jr cmdcop3
cmdcop2 ld a,(prgprzn)
        db #dd:ld l,a           ;IXL=Rechner-Prozeß-Nummer
        ld a,(dskprzn)
        db #dd:ld h,a
        ld iy,prgmsgb           ;IY=Messagebuffer
        rst #18
        or a
        db #dd:dec l            ;Test, ob Cancel gedrückt wurde
        jr nz,cmdcop3
        ld hl,(prgmsgb+8)
        ld bc,cmdcnc
        or a
        sbc hl,bc
        jp z,cmdcopb
        or a
cmdcop3 call lstful             ;Quell-Pfad holen, ZF=0 -> Eintrag gefunden, (cmdpth)=voller Pfad, (diainpbuf)=Filename [0=Länge, 1-x=Name]
        jp z,cmdcopb
        ld hl,(cmdcnt)
        inc hl
        ld (cmdcnt),hl
        ld a,h
        ld b,l
        ld c,0          ;A,BC=Counter*256
        ld de,(cmdanz)  ;DE=Gesamt
        call clcdiv     ;HL=Counter*256/Gesamt
        ld a,l
        or h
        jr z,cmdcop7
        dec hl
cmdcop7 ld a,l
        ld (copmovdat1+1),a     ;Fortschritt eintragen
        ld hl,diainpbuf
        ld a,(hl)
        inc hl
        ld c,a
        ld b,0
        inc bc
        ld de,(cmdpthz)
        ld a,(cmdcopm)
        or a
        jr nz,cmdcopa
        ldir                    ;cmdpth2=Zielpfad
cmdcopa ld e,4
        call diaupd             ;Pfade anzeigen
        ld e,5
        call diaupd
        ld e,6
        call diaupd             ;Fortschritt anzeigen
        ld a,(cmdcopt)
        or a
        jr z,cmdcope
        ld a,(cmdpth)
        ld hl,cmdpth2
        cp (hl)
        jr nz,cmdcope
        ld de,cmdpth            ;*** MOVE-SOFT -> File/Dir bewegen
cmdcopg ld a,(de)
        or a
        jr z,cmdcop2
        cp "."
        jr nz,cmdcopi
        inc de
        ld a,(de)
        dec de
        or a
        jp z,cmdcop2
        ld a,"."
cmdcopi call clcucs
        ld c,a
        ld a,(hl)
        call clcucs
        cp c
        jr nz,cmdcoph
        inc hl
        inc de
        jr cmdcopg
cmdcoph ld a,(prgbnknum)
        db #dd:ld h,a
        ld hl,(cmdpthz)
        ld (hl),0
        ld hl,cmdpth
        ld de,cmdpth2
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRMOV
        jp c,cmdcopf
        jp cmdcop2
cmdcope ld a,(cfgovrtmp)
        or a
        jr z,cmdcopj
        ld a,(prgbnknum)        ;*** Test, ob Zielfile bereits existiert
        db #dd:ld h,a
        ld hl,cmdpth2
        xor a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRPRR
        jr c,cmdcopj            ;nein -> weitermachen
        ld a,(cfgovrtmp)        ;ja -> test, ob immer skippen
        cp 2
        jp z,cmdcop2            ;immer skippen -> nächster
        call diainp9            ;Fortschritts-Fenster temporär schließen
        ld de,ovrwrtwin
        call diainp7            ;Overwrite-Fenster öffnen
        jp prgprz0
cmdcov1 call cmdcov0            ;1 -> Overwrite ONE
        jr cmdcopj
cmdcov2 call cmdcov0            ;2 -> Overwrite ALL
        xor a
        ld (cfgovrtmp),a
        jr cmdcopj
cmdcov3 call cmdcov0            ;3 -> Skip ONE
        jp cmdcop2
cmdcov4 call cmdcov0            ;4 -> Skip ALL
        ld a,2
        ld (cfgovrtmp),a
        jp cmdcop2
cmdcov0 call diainp9            ;Overwrite-Fenster schließen
        ld de,copmovwin
        jp diainp7              ;Fortschritts-Fenster wieder öffnen
cmdcopj ld a,(lstentt)
        or a
        jr z,cmdcopc
        ld a,(prgbnknum)        ;*** Verzeichnis erstellen
        db #dd:ld h,a
        ld hl,cmdpth2
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRNEW
        jr cmdcopd
cmdcopc call prgcop             ;*** Datei kopieren
cmdcopd jr c,cmdcopf
        ld a,(cmdcopt)
        or a
        jp z,cmdcop2
        call cmddel5            ;*** MOVE-HARD -> Datei/Dir löschen
        jp nc,cmdcop2
cmdcopf push af                 ;*** Fehler
        call diainp4
        pop af
        call prgerr
        jr cmdcop6
cmdcopb ld a,(cmdcopm)
        dec a
        call z,prgcop4
cmdcop5 call diainp4            ;*** Fertig
cmdcop6 xor a
        ld (cmdcopm),a
        call prgcop1            ;Buffer freigeben
        ld a,(lstakt)
        neg
        add 1
        call lstref
        ld a,(cmdcopt)
        or a
        jp z,mrknon
        ld a,(lstakt)           ;bei Move hat sich auch Quelle geändert
        call lstref
        jp prgprz0

;### CMDMOV -> Benennt ein File um oder verschiebt mehrere Files
cmdmov  call lstanz
        dec l
        jr nz,cmdmov2
        ld a,1                  ;*** Rename
        scf
        call lstnxt
        jp z,prgprz0
        ld de,cmdpth2
        ld hl,diainpbuf+1
        ld bc,256
        ldir
        dec d
        ld bc,cmdmov3
        ld hl,cmdrentxt
        ld a,255
        call diainp
        jp prgprz0
cmdmov3 scf
        call lstful
        jp z,prgprz0
        ld a,(prgbnknum)
        db #dd:ld h,a
        ld hl,cmdpth
        ld de,cmdpth2
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRREN       ;Datei/Verzeichnis umbenennen
        jp nc,shwref1
        call prgerr
        jp prgprz0
cmdmov2 ld iy,cmdmovtxt1        ;*** Move
        ld bc,cmdmov1
        ld hl,cmdmovtxt
        jp cmdcop0
cmdmov1 ld a,1
        ld (cmdcopt),a
        jp cmdcopz

;### CMDDEL -> Löscht selektierte Files
cmddel  call lstanz             ;*** Delete-Fenster vorbereiten
        jp z,prgprz0            ;keine Datei ausgewählt -> Abbruch
        push de
        pop ix                  ;Beschreibung vorbereiten
        ld de,0
        ld iy,confrmtxt1a
        call clcn32
        ld de,confrmwin
        call diainp7
        jp prgprz0
cmddel0 call diainp4            ;*** Operation ausführen
        scf
        jr cmddel2
cmddel1 or a
cmddel2 call lstful
        jp z,shwref1
        call cmddel5
        jp c,filprp7
        jr cmddel1
cmddel5 ld a,(lstentt)          ;ja nach Typ File oder Dir löschen
        or a
        ld a,FNC_FIL_DIRDEL
        jr z,cmddel4
        ld a,FNC_FIL_DIRRMD
cmddel4 ld (cmddel3),a
        ld a,(prgbnknum)
        db #dd:ld h,a
        ld hl,cmdpth
        call syscll
        db MSC_SYS_SYSFIL
cmddel3 db FNC_FIL_DIRDEL
        ret

;### CMDFOL -> Erstellt neues Verzeichnis
cmdfol  ld bc,cmdfol1           ;Dialog-Fenster aufbauen
        ld de,cmdpth
        xor a
        ld (de),a
        ld hl,cmdfoltxt
        ld a,12
        call diainp
        jp prgprz0
cmdfol1 ld a,(lstakt)           ;Operation ausführen
        or a                    ;kompletten Pfad vorbereiten
        ld hl,pthdev1
        jr z,cmdfol2
        ld hl,pthdev2
cmdfol2 ld de,diainpbuf
        push de
        ld bc,256
        ldir
        pop de
        ld hl,cmdpth
        push de
        call stradd
        pop hl
        ld a,(prgbnknum)
        db #dd:ld h,a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DIRNEW       ;Verzeichnis erstellen
        jp c,filprp7
        jp shwref1

;### CMDSWP -> Tauscht linke mit rechter Liste
cmdswp  ld hl,pthlen1       ;Liste 1 -> Buffer
        ld de,diainpbuf
        ld bc,1+256
        ldir
        ld hl,pthmsk1
        ld bc,9
        ldir
        ld hl,lstlay1
        ld bc,65
        ldir
        ld hl,pthlen2       ;Liste 2 -> Liste 1
        ld de,pthlen1
        ld bc,1+256
        ldir
        ld hl,pthmsk2
        ld de,pthmsk1
        ld bc,9
        ldir
        ld hl,lstlay2
        ld de,lstlay1
        ld bc,65
        ldir
        ld hl,diainpbuf     ;Buffer  -> Liste 2
        ld de,pthlen2
        ld bc,1+256
        ldir
        ld de,pthmsk2
        ld bc,9
        ldir
        ld de,lstlay2
        ld bc,65
        ldir
        jp prgprz3          ;beide Listen updaten

;### CMDEQU -> Setzt inaktive Liste mit aktiver gleich
cmdequ  ld a,(lstakt)
        or a
        ld hl,pthlen1
        ld de,pthlen2
        ld bc,pthmsk1
        ld ix,pthmsk2
        jr z,cmdequ1
        ex de,hl
        ld bc,pthmsk2
        ld ix,pthmsk1
cmdequ1 push bc
        push ix
        ld bc,1+256
        ldir
        pop hl
        pop de
        ld bc,9
        ldir
        or a
        ld hl,lstlay1
        ld de,lstlay2
        jr z,cmdequ2
        ex de,hl
cmdequ2 ld bc,65
        ldir
        neg
        add 1               ;A=upgedatete Liste
        push af
        call pthupd
        pop af
        call lstref
        jp prgprz0


;==============================================================================
;### "SHOW" FUNKTIONEN ########################################################
;==============================================================================

;### SHWALL -> Zeigt alle File an
swhallm db "*.*":ds 9-3
shwall  ld a,(lstakt)
        or a
        ld de,pthmsk1
        jr z,swhall1
        ld de,pthmsk2
swhall1 ld hl,swhallm
        ld bc,9
        ldir
        ld ix,prgwinmen4
        set 1,(ix+2)
        res 1,(ix+10)
        jr shwref

;### SHWREF -> Liest Directory neu ein
shwref  ld a,(lstakt)
        call pthupd
shwref1 ld a,(lstakt)
        call lstref
        jp prgprz0

;### SHWDEF -> Zeigt Files mit zuvor definierter Maske an
shwdef  ld a,(lstakt)
        or a
        push af
        call lstswp6
        pop af
        ld de,pthmsk1
        ld hl,pthcst1
        jr z,shwdef1
        ld de,pthmsk2
        ld hl,pthcst2
shwdef1 ld bc,9
        ldir
        ld ix,prgwinmen4
        res 1,(ix+2)
        set 1,(ix+10)
        jr shwref

;### SHWCST -> Definiert neue Maske und zeigt entsprechende Files an
shwcst  ld a,(lstakt)           ;Dialog-Fenster aufbauen
        or a
        ld de,pthcst1
        jr z,shwcst0
        ld de,pthcst2
shwcst0 ld bc,shwdef
        ld a,9
        jp mrksel2

;### SHWNAM -> Sortiert nach Namen
shwnam  ld ix,prgwinmen4
        set 1,(ix+34)
        res 1,(ix+42)
        res 1,(ix+50)
        call shwnam0
        ld a,64+0
        jr shwnam1
shwnam0 call lstget
        ex de,hl
        ld bc,9
        add hl,bc
        rst #20:dw jmp_bnkrbt
        dec hl
        ld c,a
        ld a,b
        ret
shwnam1 set 6,a
        ld b,a
        ld a,c
        rst #20:dw jmp_bnkwbt
        jp mrkall2

;### SHWSIZ -> Sortiert nach Größe
shwsiz  ld ix,prgwinmen4
        res 1,(ix+34)
        set 1,(ix+42)
        res 1,(ix+50)
        call shwnam0
        ld a,64+1
        jr shwnam1

;### SHWDAT -> Sortier nach Datum
shwdat  ld ix,prgwinmen4
        res 1,(ix+34)
        res 1,(ix+42)
        set 1,(ix+50)
        call shwnam0
        ld a,64+2
        jr shwnam1

;### SHWREV -> Kehrt Sortierung um
shwrev  call shwnam0
        xor 128
        ld iy,prgwinmen4
        jp p,swhrev1
        set 1,(iy+66)
        jr shwnam1
swhrev1 res 1,(iy+66)
        jr shwnam1


;==============================================================================
;### "CONFIG" FUNKTIONEN ######################################################
;==============================================================================

;### CFGSET -> Config-Dialog
cfgset  ld ix,(cfglin)
        ld iy,configbuf1
        call cfgset2
        ld ix,(cfgmem)
        ld iy,configbuf2
        call cfgset2
        ld ix,(cfgbuf)
        ld iy,configbuf3
        call cfgset2
        ld a,(cfgovr)
        ld (configovr),a
        ld a,(cfgatr)
        cp 8
        ld a,1
        jr z,cfgset3
        xor a
cfgset3 ld (configatr),a
        ld de,configwin
        call diainp7
        jp prgprz0
cfgset2 ld de,0
        push iy
        call clcn32
        ex (sp),iy
        pop hl
        db #fd:ld e,l
        db #fd:ld d,h
        or a
        sbc hl,de
        inc l
        ld (iy-6),l
        ld (iy-10),l
        ret
cfgset1 call diainp4            ;*** neue Config übernehmen
        ld a,(configatr)
        or a
        ld a,8+2+4
        jr z,cfgset8
        ld a,8
cfgset8 ld (cfgatr),a
        ld a,(configovr)
        ld (cfgovr),a
        ld ix,configbuf1
        xor a
        ld bc,50
        ld de,1000
        call clcr16
        jr c,cfgset4
        ld (cfglin),hl
cfgset4 ld ix,configbuf2
        xor a
        ld bc,1024
        ld de,16384
        call clcr16
        jr c,cfgset5
        ld (cfgmem),hl
cfgset5 ld ix,configbuf3
        xor a
        ld bc,1024
        ld de,16384
        call clcr16
        jr c,cfgset6
        ld (cfgbuf),hl
cfgset6 ld ix,prgmemtab
        ld b,4
cfgset7 push bc
        push ix
        ld a,(ix+0)
        ld (ix+0),0
        ld l,(ix+1)
        ld h,(ix+2)
        ld c,(ix+3)
        ld b,(ix+4)
        ld (ix+3),0
        ld (ix+4),0
        rst #20:dw jmp_memfre
        pop ix
        ld bc,5
        add ix,bc
        pop bc
        djnz cfgset7
        call lstini
        jp nc,prgprz6
        ;...error -> memory full
        jp prgend

;### CFGINI -> Generiert Config-Pfad und lädt Config
cfgnam  db "symcmder.ini",0:cfgnam0
cfgpth  dw 0

cfgini  ld hl,(prgcodbeg)
        ld de,prgcodbeg
        dec h
        add hl,de           ;HL=CodeEnde=Pfad
        ld (cfgpth),hl
        ld e,l
        ld d,h              ;DE=HL
        ld b,255
cfgini1 ld a,(hl)           ;Pfad-Ende suchen
        or a
        jr z,cfgini2
        inc hl
        djnz cfgini1
        jr cfgini4
        ld a,255
        sub b
        jr z,cfgini4
        ld b,a
cfgini2 dec hl              ;Filename-Anfang suchen
        ld a,(hl)
        cp "/"
        jr z,cfgini3
        cp "\"
        jr z,cfgini3
        cp ":"
        jr z,cfgini3
        djnz cfgini2
        jr cfgini4
cfgini3 inc hl
        ex de,hl
cfgini4 ld hl,cfgnam        ;Programm-Filename mit Config-Filename ersetzen
        ld bc,cfgnam0-cfgnam
        ldir
        ld hl,(cfgpth)      ;Config-File öffnen
        ld a,(prgbnknum)
        db #dd:ld h,a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOPN
        ret c
        ld de,(prgbnknum)   ;Config laden
        ld hl,cfgbeg
        ld bc,cfgend-cfgbeg
        push af
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        pop af              ;Config-File schließen
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        ret

;### CFGSAV -> Speichert Config-Datei
cfgsav  ld hl,(cfgpth)      ;Config-File öffnen
        ld a,(prgbnknum)
        db #dd:ld h,a
        xor a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILNEW
        jp c,prgprz0
        ld de,(prgbnknum)   ;Config speichern
        ld hl,cfgbeg
        ld bc,cfgend-cfgbeg
        push af
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOUT
        pop af              ;Config-File schließen
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        jp prgprz0


;==============================================================================
;### INIT ROUTINEN ############################################################
;==============================================================================

;### DEVINI -> Device-Dropdowns initialisieren
devini  ld hl,jmp_sysinf
        ld e,3
        ld ix,cfgdev
        rst #28                 ;Device-Config laden
        ld ix,cfgdev
        ld bc,8*256
        ld iy,prgtabdev
devini1 ld a,(ix+0)
        or a
        jr z,devini2
        inc c
        ld (iy+0)," "
        ld (iy+1),"["
        ld (iy+2),a
        ld (iy+3),"]"
        ld (iy+4),0
        ld de,5
        add iy,de
devini2 ld de,16
        add ix,de
        djnz devini1
        ld a,c
        ld (prgobjdeva),a
        ld (prgobjdevb),a
        ld a,(prgtabdev+2)
        ld (pthdev1),a
        ld (pthdev2),a
        ret


;==============================================================================
;### EINGABE ROUTINEN #########################################################
;==============================================================================

;### INPFIL -> Holt angewähltes File
;### Ausgabe    (diainpbuf)=Filename [0=Länge, 1-x=Name], CF=1 Liste ist leer
inpfil  call lstget     ;A=Bank
        scf
        ret z
        ex de,hl        ;DE=Zeilendaten
        ld bc,12
        add hl,bc
        rst #20:dw jmp_bnkrwd
        ld l,c
        ld h,b          ;HL=Eintragsnummer
        ld c,1
        call lstent     ;(diainpbuf)=Filename [0=Länge, 1-x=Name]
        or a
        ret

;### INPCLR -> Eingabezeile löschen
inpclr  call inpclr0
        jp prgprz0
inpclr0 xor a
        ld (prgobjinpb),a
        ld l,a
        ld h,a
        ld (prgobjinp1+2),hl
        ld (prgobjinp1+4),hl
        ld (prgobjinp1+6),hl
        ld (prgobjinp1+8),hl
inpclr1 ld e,19
        jp msgsnd0

;### INPRET -> File aufrufen
inpret  ld hl,jmp_keysta
        rst #28
        bit 2,e
        jp nz,filprp
        bit 1,e
        jr z,inpret3
        call inpfil             ;*** Control+Return -> File in Eingabezeile übernehmen
        jp c,prgprz0
        ld hl,prgobjinpb
        ld a,(prgobjinp1+8)
        ld c,a
        ld b,0
        add hl,bc
        or a
        jr z,inpret1
        inc a
        jp z,prgprz0
        inc b
inpret1 ex de,hl
        ld hl,diainpbuf
        add (hl)
        jp c,prgprz0
        ld (prgobjinp1+8),a
        dec b
        jr nz,inpret2
        dec de
        ld a,(de)
        inc de
        cp 32
        jr nz,inpret6
        ld a,(prgobjinp1+8)
        dec a
        ld (prgobjinp1+8),a
        jr inpret2
inpret6 ld a,32
        ld (de),a
        inc de
inpret2 xor a
        ld c,(hl)
        ld b,a
        inc hl
        ldir
        ld (de),a
        ld a,(prgobjinp1+8)
        ld (prgobjinp1+4),a
        call inpclr1
        jp prgprz0
inpret3 ld a,(prgobjinp1+8)     ;*** Return -> File starten
        or a
        jr nz,inpret5
        call inpfil             ;*** File aus Liste starten
        jp c,prgprz0
inpret4 call lstful0
        jp filopn0
inpret5 ld hl,prgobjinpb        ;*** File aus Eingabe starten
        ld de,diainpbuf
        ld a,(prgobjinp1+8)
        ld (de),a
        inc de
        ld bc,256
        ldir
        call inpclr0
        jr inpret4

;### INPFOC -> Eingabezeile bekommt Focus, Zeichen in Tastaturbuffer
;### Eingabe    C=Zeichen
inpfoc  ld a,(prgwingrp+14)
        cp 20
        jp z,prgprz0
        ld a,20
        jr lstfoc3


;==============================================================================
;### LISTEN ROUTINEN ##########################################################
;==============================================================================

;### LSTFOCM -> User hat Focus über Click geändert
lstfocm ld a,(prgwingrp+14)
        cp 6
        jp c,prgprz0
        cp 7+1
        jp nc,prgprz0
        sub 6
        ld d,a
        ld a,(lstakt)
        cp d
        jp z,prgprz0
        call lstswp
        jp prgprz0

;### LSTFOCK -> User hat Focus über Tab geändert
lstfock ld a,(prglstfoc)
        xor 1
        ld (prglstfoc),a
        call lstfoc4
        jp prgprz0

;### LSTFOC -> letzte Focus-Liste bekommt diesen Zurück, Zeichen in Tastaturbuffer
;### Eingabe    C=Zeichen
lstfoc  ld a,(prglstfoc)
        add 6
lstfoc3 ld (prgwingrp+14),a
        ld a,c
        rst #20:dw #814b
        jp prgprz0

;### LSTFOCA -> Liste 1 soll Focus bekommen
lstfoca call lstfoc1
        jp prgprz0
lstfoc1 xor a
        jr lstfoc0

;### LSTFOCB -> Liste 2 soll Focus bekommen
lstfocb call lstfoc2
        jp prgprz0
lstfoc2 ld a,1
lstfoc0 ld (prglstfoc),a
        add 6
        ld hl,prgwingrp+14
        cp (hl)
        ret z
        ld (hl),a
        sub 6
lstfoc4 xor 1
        ld (lstakt),a
        ld de,256*20+256-2
        call msgsnd0
        jr lstswp

;### LSTCLKA -> Click auf Liste 1
lstclka ld d,0
        jr lstclk

;### LSTCLKB -> Click auf Liste 2
lstclkb ld d,1
lstclk  cp 2
        jp z,filopn
        ld a,(lstakt)
        cp d
        jr z,lstclk1
        push de
        call lstswp
        pop af
lstclk1 push af
        call lstswp0
        pop af
        scf
        call lstinf
        jp prgprz0

;### LSTSWP -> Wechselt Fokus auf andere Liste
lstswp  ld a,(lstakt)
        xor 1
        ld (lstakt),a
        push af
        call lstswp6
        ld (ix+2),3+0+128
        ld (iy+2),2+4+128
        ld hl,swhallm
        ld b,4
        ld c,1
lstswp3 ld a,(de)
        cp (hl)
        jr nz,lstswp4
        inc hl
        inc de
        djnz lstswp3
        ld c,1+2
lstswp4 ld a,c
        ld (prgwinmen4+2),a         ;Masken-Select anpassen
        xor 2
        ld (prgwinmen4+2+8),a
        ld de,256*3+256-2           ;Focus anzeigen
        call msgsnd0
        pop af
        jp pthupd

lstswp6 ld hl,pthcst1       ;*** Menu an Costum maske anpassen (zf=1 list1, zf=0 list2)
        ld de,pthmsk1
        ld ix,prgobjdsc1
        ld iy,prgobjdsc2
        jr z,lstswp5
        ld hl,pthcst2
        ld de,pthmsk2
        ld ix,prgobjdsc2
        ld iy,prgobjdsc1
lstswp5 push de
        ld de,prgwinmen4tx2+6       ;Custom-Maske anpassen
        ld bc,9
        ldir
        pop de
        ret

lstswp0 call shwnam0        ;*** Menu an Sortierung anpassen
        ld c,a                      ;A=Sort-Byte (bit0-5=spalte, b7=reihenfolge)
        rlca
        rlca
        and 2
        add 1
        ld ix,prgwinmen4+2
        ld (ix+64),a                ;Sort-Reihenfolge anpassen
        ld a,c
        and 63
        inc a
        ld b,3
        ld de,8
lstswp1 ld (ix+32),1                ;Sort-Spalte anpassen
        dec a
        jr nz,lstswp2
        ld (ix+32),1+2
lstswp2 add ix,de
        djnz lstswp1
        ret

;### LSTANZ -> Holt Anzahl selektierter Files in der aktuellen Liste
;### Ausgabe    DE=Anzahl, ZF=1 -> Liste ist leer, L=1 -> nichts selektiert, aber Cursor wird als Selektion genommen
;### Verändert  AF,BC,HL,IX,IY
lstanz  call lstget
        ld l,0
        ld a,(lstakt)
        or a
        ld de,(lstanz1)
        jr z,lstanz3
        ld de,(lstanz2)
lstanz3 ld a,e
        or d
        ret nz
        ld a,c          ;keine Files selektiert -> wenn Liste nicht leer "1" zurückgeben, da dann angewählter zählt
        or b
        ret z
        inc l
        inc e
        ret

;### LSTFUL -> Holt nächsten vollen File-Pfad aus aktueller Liste
;### Eingabe    CF=1 -> von vorne starten
;### Ausgabe    ZF=0 -> Eintrag gefunden, (cmdpth)=voller Pfad, (diainpbuf)=Filename [0=Länge, 1-x=Name]
lstful  ld a,1
        call lstnxt
        ret z
lstful0 ld a,(lstakt)
        or a
        ld hl,pthdev1
        jr z,lstful1
        ld hl,pthdev2
lstful1 ld de,cmdpth
        push de
        ld bc,256
        ldir
        pop hl
        call strlen
        ex de,hl
        ld hl,diainpbuf+1
        ld a,(diainpbuf)
        ld c,a
        ld b,0
        inc bc
        ldir
        or a
        ret

;### LSTNXT -> Holt nächstes (un)selektiertes File aus aktueller Liste
;### Eingabe    A=Typ (0=unselektiert holen, 1=selektiert holen), CF=1 -> von vorne starten
;### Ausgabe    ZF=0 -> Eintrag gefunden, HL=gefundene Position, (diainpbuf)=Filename [0=Länge, 1-x=Name], DE=Zeilendaten,
;###                    (lstnxtb)=Bank, CF=1 Directory
lstnxtm db 2    ;2=selektierte Files holen, 1=unselektierte Files holen
lstnxtb db 0    ;Bank
lstnxtz dw 0    ;Anfang Zeilendaten
lstnxtp dw 0    ;Position
lstnxtl dw 0    ;noch übrige Zeilen
lstnxth dw 0

lstnxt  inc a
        ld (lstnxtm),a
        jr nc,lstnxt1
        call lstget         ;A=Bank, DE=Kopf, HL=Zeilen-Daten, BC=Anzahl Zeilen, IXH=Control, ZF=1 -> keine Zeilen
        ret z
        ld (lstnxth),de
        ld (lstnxtb),a
        ld (lstnxtl),bc
        ld (lstnxtz),hl
        ex de,hl
        ld hl,0
        ld (lstnxtp),hl
        ld a,(lstnxtm)
        cp 2
        jr nz,lstnxt2
        ld a,(lstakt)       ;selektierte suchen -> prüfen, ob spezialfall "keine ausgewählt -> cursor nehmen"
        or a
        ld hl,(lstanz1)
        jr z,lstnxt5
        ld hl,(lstanz2)
lstnxt5 ld a,l
        or h
        jr nz,lstnxt1
        ld (lstnxtl),hl     ;*** kein eintrag selektiert -> cursor-eintrag nehmen
        ld hl,(lstnxth)
        ld bc,12
        add hl,bc
        ld a,(lstnxtb)
        rst #20:dw jmp_bnkrwd
        ld l,c
        ld h,b
        inc bc
        ld (lstnxtp),bc
        ld de,(lstnxtz)
        ld c,1
        push hl
        push de             ;A=Bank, DE=Zeilen-Daten, HL=Eintragsnummer, C=Typ (1=Name, 2=Name nur holen, wenn markiert, 3=Länge)
        call lstent         ;(diainpbuf)=Daten (Filename [0=Länge, 1-x=Name], DWLänge oder leer), ZF=Markierungs-Flag (NZ=markiert)
        pop de
        pop hl
        ld a,1
        or a
        ret
lstnxt1 ld de,(lstnxtz)     ;*** reguläre Suche
        ld bc,(lstnxtl)
        ld hl,(lstnxtp)
lstnxt2 ld a,c
        or b
        ret z
        push bc
        push de
        push hl
        ld bc,(lstnxtm)
        ld a,b
        call lstent
        pop hl
        pop de
        pop bc
        inc hl
        dec bc
        ld a,(lstnxtm)      ;2->bei z loop, 1->bei nz loop
        jr z,lstnxt3
        dec a
        jr z,lstnxt2
        jr lstnxt4
lstnxt3 dec a
        jr nz,lstnxt2
        inc a
lstnxt4 ld (lstnxtp),hl
        ld (lstnxtl),bc
        dec hl
        ret

;### LSTINF -> Updated Infotext mit Anzahl und Filegröße einer Liste
;### Eingabe    A=Liste, CF=1 Test, ob nur SelektAnzahl anders ist
lstinft db " files",0
lstinf  push af
        call lstsiz
        pop af
        jr nc,lstinf3
        or a
        ld iy,prgobjtxt8
        jr z,lstinf4
        ld iy,prgobjtxt9
lstinf4 push ix
        pop hl
        ld e,(iy-2)
        ld d,(iy-1)
        or a
        sbc hl,de
        ret z
lstinf3 or a
        ld iy,prgobjtxt8
        jr z,lstinf1
        ld iy,prgobjtxt9
lstinf1 push af
        db #dd:ld a,l
        ld (iy-2),a
        db #dd:ld a,h
        ld (iy-1),a         ;Anzahl Elemente merken
        push bc
        push ix
        ld ix,lstsizszs
        call lstinf2        ;Select-Filelänge
        ld (iy+0),"/"
        inc iy
        ld ix,lstsizsza
        call lstinf2        ;Gesamt-Filelänge
        ld (iy+0)," "
        ld (iy+1),"i"
        ld (iy+2),"n"
        ld (iy+3)," "
        ld bc,4
        add iy,bc
        pop ix
        ld de,0
        call clcn32         ;Anzahl Select
        inc iy
        ld (iy+0),"/"
        inc iy
        pop ix
        ld de,0
        call clcn32         ;Anzahl Gesamt
        push iy
        pop de
        inc de
        ld hl,lstinft
        ld bc,7
        ldir
        pop af
        add 9
        ld e,a
        ld c,MSC_DSK_WININH
        ld a,(prgwin)
        ld b,a
        jp msgsnd
lstinf2 ld e,(ix+0)
        ld d,(ix+1)
        ld hl,1023
        add hl,de
        ex de,hl
        ld c,(ix+2)
        ld b,(ix+3)
        ld hl,0
        adc hl,bc       ;HL,DE=Zahl+1023
        ld e,h
        ld a,l          ;EAD=(Zahl+1023)/256
        srl e:rra:rr d
        srl e:rra:rr d
        db #dd:ld l,d
        db #dd:ld h,a
        ld d,0
        call clcn32
        ld (iy+1),"k"
        inc iy
        inc iy
        ret

;### LSTSIZ -> Holt Anzahl gesamt und selektierter Files und jeweils die Längensummen
;### Eingabe    A=Liste
;### Ausgabe    BC=Anzahl aller Files, IX=Anzahl selektierter Files, (lstsizsza)=Größe aller Files, (lstsizszs)=Größe selektierter Files
;### Verändert  AF,DE,HL,IX
lstsizsza   ds 4
lstsizszs   ds 4

lstsiz  ld hl,lstsizsza     ;Größen auf 0 setzen
        ld de,lstsizsza+1
        ld bc,8-1
        ld (hl),0
        ldir
        call lstget0        ;A=Bank, DE=Kopf, HL=Zeilen-Daten, BC=Anzahl Zeilen, IXH=Control, ZF=1 -> keine Zeilen
        ld ix,0
        ret z
        push bc
        ex de,hl
        ld hl,0
lstsiz1 push af
        push bc
        push de
        push hl
        ld c,3
        call lstent
        ld bc,(diainpbuf)
        ld de,(diainpbuf+2)
        jr z,lstsiz2
        inc ix
        ld hl,(lstsizszs)       ;Größe von selektierten Files
        add hl,bc
        ld (lstsizszs),hl
        ld hl,(lstsizszs+2)
        adc hl,de
        ld (lstsizszs+2),hl
lstsiz2 ld hl,(lstsizsza)       ;Größe aller Files
        add hl,bc
        ld (lstsizsza),hl
        ld hl,(lstsizsza+2)
        adc hl,de
        ld (lstsizsza+2),hl
        pop hl
        pop de
        pop bc
        pop af
        inc hl
        dec bc
        inc c
        dec c
        jr nz,lstsiz1
        inc b
        dec b
        jr nz,lstsiz1
        pop bc
        ret

;### LSTENT -> Holt Daten eines Files
;### Eingabe    A=Bank, DE=Zeilen-Daten, HL=Eintragsnummer, C=Typ (1=Name, 2=Name nur holen, wenn markiert, 3=Länge)
;### Ausgabe    (diainpbuf)=Daten (Filename [0=Länge, 1-x=Name], DWLänge oder leer), ZF=Markierungs-Flag (NZ=markiert),
;###            CF,(lstentt)=1 Directory
;### Verändert  AF,BC,DE,HL
lstentp dw 0
lstentt db 0
lstent  ld (lstentp),hl
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,de
        inc hl
        rst #20:dw jmp_bnkrbt
        push bc
        dec c
        jr nz,lstent3
lstent4 rst #20:dw jmp_bnkrwd   ;*** Name holen
        ld hl,prgbnk
        or (hl)
        ld l,c
        ld h,b
        ld de,diainpbuf+1
        ld bc,254
        rst #20:dw jmp_bnkcop
        ld hl,(diainpbuf+1)
        ld bc,256*"["+" "
        or a
        sbc hl,bc
        jr nz,lstent1
        ld hl,diainpbuf+3       ;* Directory
        ld de,diainpbuf+1
        ld bc,254-2
        ldir
        ld hl,diainpbuf+1
        call strlen
        dec hl
        ld (hl),0
        dec c
        scf
        ld a,1
        jr lstent2
lstent1 ld hl,diainpbuf+1       ;* File
        call strlen
        xor a
lstent2 ld (lstentt),a
        ld a,c
        ld (diainpbuf),a
lstent6 pop bc
        bit 7,b
        ret
lstent3 dec c
        jr nz,lstent5
        bit 7,b                 ;*** Name nur holen, wenn markiert
        jr nz,lstent4
        pop bc
        ret
lstent5 inc hl                  ;*** Länge holen
        inc hl
        rst #20:dw jmp_bnkrwd
        ld hl,prgbnk
        or (hl)
        ld l,c
        ld h,b
        ld de,diainpbuf
        ld bc,4
        rst #20:dw jmp_bnkcop
        jr lstent6

;### LSTGET -> Holt Daten der aktuellen Liste
;### Ausgabe    A=Bank, DE=Kopf, HL=Zeilen-Daten, BC=Anzahl Zeilen, IXH=Control, ZF=1 -> keine Zeilen
;### Verändert  IY
lstget  ld a,(lstakt)
lstget0 or a
        ld iy,lstbnk1
        jr z,lstget1
        ld iy,lstbnk2
lstget1 add 5
        db #dd:ld h,a           ;IXH=Control
        ld a,(iy+0)             ;A=Bank
        ld l,(iy+1)
        ld h,(iy+2)
        ld e,l
        ld d,h                  ;DE=Kopf
        rst #20:dw jmp_bnkrwd
        push bc
        inc hl
        inc hl
        rst #20:dw jmp_bnkrwd
        ld l,c
        ld h,b                  ;HL=Zeilendaten
        pop bc                  ;BC=Anzahl Zeilen
        inc b
        dec b
        ret nz
        inc c
        dec c                   ;ZF=Flag, ob 0 Zeilen
        ret

;### LSTINI -> Speicher für Directory-Listen reservieren und vorbereiten
;### Ausgabe    CF=1 -> Speicher voll, CF=0 -> alles ok
;### Veraendert AF,BC,DE,HL,IX,IY
lsttab  dw 0,0, 00 ,0,256*64+3, 00 ,0,3
lsttab1 dw 4*0+0,63,00,0, 4*3+1,41,00,0, 4*0+0,69,00,0
lsttab3 db "Name",0,      "Size",0,      "Date",0:lsttab2
lstlen  dw 0

lstbnk1 db 0    ;Bank             der linken  Tabelle
lstadr1 dw 0    ;Adresse Transfer der linken  Tabelle
lstdat1 dw 0    ;Adresse Daten    der linken  Tabelle

lstbnk2 db 0    ;Bank             der rechten Tabelle
lstadr2 dw 0    ;Adresse Transfer der rechten Tabelle
lstdat2 dw 0    ;Adresse Data     der rechten Tabelle

lstakt  db 0    ;aktuelle Liste (0=links, 1=rechts)

lstlay1 db 0    ;aktuelle Ebene
lstpos1 ds 32*2 ;Cursor Position für jeweilige Ebene
lstlay2 db 0
lstpos2 ds 32*2

lstini  call lstini1
        ld hl,(lstlen)
        ld (5*0+prgmemtab+3),hl
        ld hl,(cfgmem)
        ld (5*1+prgmemtab+3),hl
        ret c
        call lstini7
        ld a,(lstbnk2)
        ld hl,(lstadr2)
        ld (prgwinobj1-1),a
        ld (prgwinobj1),hl
        ld hl,lstbnk2
        ld de,lstbnk1
        ld bc,5
        ldir
        ld hl,5*0+prgmemtab
        ld de,5*2+prgmemtab
        ld bc,5*2
        ldir
        call lstini1
        ret c
        call lstini7
        ld a,(lstbnk2)
        ld hl,(lstadr2)
        ld (prgwinobj2-1),a
        ld (prgwinobj2),hl
        ret
lstini7 ld a,(lstbnk2)              ;reservierten Speicher fürs Betriebssystem merken
        ld (5*0+prgmemtab+0),a
        ld (5*1+prgmemtab+0),a
        ld hl,(lstadr2)
        ld (5*0+prgmemtab+1),hl
        ld hl,(lstdat2)
        ld (5*1+prgmemtab+1),hl
        ret
;CF=0 ok -> lstbnk2/lstadr2/lstdat2=Adresse von reserviertem Speicher, lstlen=Länge von Transferspeicher
lstini1 ld hl,(cfglin)
        add hl,hl
        add hl,hl
        add hl,hl
        ld bc,lsttab2-lsttab
        add hl,bc
        ld (lstlen),hl
        ld c,l
        ld b,h                  ;BC=Zeilen*8 + vorangehende Datenstruktur
        xor a
        ld e,2
        rst #20:dw jmp_memget   ;Transfer-Speicher in beliebiger Bank reservieren
        jr nc,lstini3
        ret                     ;in keiner Bank Speicher frei -> Ende
lstini2 ld bc,(lstlen)          ;in nächster Bank Transfer-Speicher reservieren
        ld e,2
        push af
        rst #20:dw jmp_memget
        jr c,lstini4            ;nicht frei -> nächste Bank
        pop bc
lstini3 ld (lstbnk2),a          ;frei -> Bank und Adresse von Transfer-Speicher merken
        ld (lstadr2),hl
        ld bc,(cfgmem)
        ld e,1
        push af
        push hl
        rst #20:dw jmp_memget   ;Data-Speicher in selber Bank reservieren
        pop de
        pop bc
        jr nc,lstini5           ;erfolgreich -> Speicherreservierung fertig
        ld a,b
        ex de,hl
        ld bc,(lstlen)
        push af
        rst #20:dw jmp_memfre   ;nein -> Transferspeicher wieder freigeben
lstini4 pop af
        inc a                   ;in nächster Bank versuchen
        cp 15+1
        jr c,lstini2
        scf                     ;keine Bank mehr übrig -> Ende
        ret
lstini5 ld (lstdat2),hl         ;Speicher erfolgreich reserviert -> Adresse von Data-Speicher merken
        ld hl,(lstadr2)
        push hl
        ld bc,lsttab2-lsttab
        add hl,bc
        ld (lsttab+4),hl        ;ZeilenInfo-Zeiger setzen
        ld bc,lsttab1-lsttab2
        add hl,bc
        ld (lsttab+10),hl       ;SpaltenInfo-Zeiger setzen
        ld bc,lsttab3-lsttab1
        add hl,bc               ;HL=Adresse des ersten Spaltentitels
        ld a,3
        ld ix,lsttab1
        ld d,0
lstini6 ld (ix+4+0),l           ;SpaltenName-Zeiger setzen
        ld (ix+4+1),h
        ld e,5
        add hl,de
        ld e,8
        add ix,de
        dec a
        jr nz,lstini6
        pop de
        ld a,(lstbnk2)
        ld hl,prgbnk
        or (hl)
        rlca:rlca:rlca:rlca
        ld hl,lsttab
        ld bc,lsttab2-lsttab
        rst #20:dw jmp_bnkcop       ;tabellen-objekt kopieren
        or a
        ret

;### LSTREF -> Liste neu laden
;### Eingabe    A=Nummer (0=links, 1=rechts)
lstrefb db 0
lstrefa dw 0
lstrefp dw 0        ;Position auf die Cursor gesetzt werden soll
lstrefn db 0

lstref  ld (lstrefn),a
        ld iy,lstbnk1       ;IY=Speicherdaten von linker Liste
        ld hl,pthful1       ;HL=Pfad von linker Liste
        or a
        jr z,lstref1
        ld iy,lstbnk2       ;IY=Speicherdaten von rechter Liste
        ld hl,pthful2       ;HL=Pfad von rechter Liste
lstref1 ld (lstref4+2),hl
        add 5               ;A=Listenobjekt-Nummer
        push af
        push hl
        ld a,(cfgatr)
        db #dd:ld l,a       ;IXL=Attribut-Flags
        db #dd:ld h,3       ;IXH=Spalten-Flags
        ld a,(iy+0)         ;A=Bank
        ld l,(iy+1)
        ld h,(iy+2)         ;HL=TabAdr
        ld (lstrefb),a
        ld (lstrefa),hl
        ld bc,lsttab2-lsttab
        add hl,bc
        ld c,l
        ld b,h              ;BC=TransferSpeicherbeginn
        ld l,(iy+3)
        ld h,(iy+4)
        ld e,l
        ld d,h              ;DE,HL=DatAdr
        rst #20:dw jmp_bnkwwd
        ld bc,(cfglin)
        rst #20:dw jmp_bnkwwd
        ld bc,(cfgmem)
        ld hl,prgbnk
        or (hl)             ;A4-7=eigene Bank=Pfadbank
        pop hl              ;HL=Pfad
        ld iy,0
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_DEVDIR   ;Verzeichnis laden
        pop de              ;D=Control
        jr c,lstref2
        ld c,l              ;Anzahl Zeilen eintragen
        ld b,h
        ld a,(lstrefb)
        ld hl,(lstrefa)
        rst #20:dw jmp_bnkwwd
        ld bc,0             ;Position auf erste Zeile
        rst #20:dw jmp_bnkwwd
        ld bc,5             ;Sortierung anfordern
        add hl,bc
        rst #20:dw jmp_bnkrbt
        dec hl
        set 6,b
        rst #20:dw jmp_bnkwbt
        inc hl
        inc hl
        push hl
        ld bc,(lstrefp)
        ld hl,0
        ld (lstrefp),hl
        pop hl
        rst #20:dw jmp_bnkwwd
        push de
        ld a,d
        sub 5
        call lstinf         ;File/Size-Info refreshen
        pop de
lstref3 ld e,d              ;Liste aufbauen
        ld c,MSC_DSK_WINDIN
        ld a,(prgwin)
        ld b,a
        jp msgsnd
lstref2 push de             ;*** Fehler beim Directory-Einlesen
        push af
        ld bc,0
        ld a,(lstrefb)
        ld hl,(lstrefa)
        rst #20:dw jmp_bnkwwd
        pop af
        push af
        call prgerr
        pop af
        pop de
        cp 12
        jr nz,lstref3
lstref4 ld ix,0             ;### directory doesn't exist -> go to root
        db #dd:dec h
        xor a
        ld (ix+3),a
        inc a
        ld (ix-1),a
        ld a,(lstrefn)
        push af
        call pthupd
        pop af
        jp lstref

;### PTHUPD -> Pfad neu zusammensetzen und Anzeigen updaten
;### Eingabe    A=Nummer (0=links, 1=rechts)
pthupdf db 0
pthupd  ld b,a
        ld a,(lstakt)
        sub b
        ld (pthupdf),a
        ld a,b
        or a
        ld hl,pthlen1
        ld ix,prgobjdeva
        jr z,pthupd1
        ld hl,pthlen2
        ld ix,prgobjdevb
pthupd1 inc a               ;A=Objekt-Nummer
        push af
        ld c,(hl)
        ld b,0              ;BC=(pthlen)
        inc c
        inc c
        inc hl              ;HL=pthdev
        push hl
        ld a,(hl)           ;A=Device
        ld e,l
        ld d,h
        inc d               ;DE=pthdev+256=pthful
        push hl
        ldir                ;Device+Pfad kopieren
        pop hl
        ld bc,pthmsk1-pthdev1
        add hl,bc           ;HL=pthmsk
        ld bc,9
        ldir                ;Maske kopieren
        pop hl
        ld e,a
        ld a,(pthupdf)
        or a
        jr nz,pthupd4
        ld (prgobjdsc5),hl  ;falls aktuelle Liste, Eingabe-Pfad updaten
pthupd4 ld a,e
        ld hl,prgtabdev+2   ;Device im Dropdown auswählen
        ld de,5
        ld b,8
pthupd2 cp (hl)
        jr z,pthupd3
        add hl,de
        djnz pthupd2
        ld b,8
pthupd3 ld a,8
        sub b
        ld (ix+12),a
        pop af
        ld e,a
        ld c,MSC_DSK_WININH
        ld a,(prgwin)
        ld b,a
        push bc
        push de
        call msgsnd         ;Dropdown aktualisieren
        pop de
        pop bc
        inc e:inc e
        push bc
        call msgsnd         ;Pfad aktualisieren
        pop bc
        ld a,(pthupdf)      ;Input-Pfad aktualisieren, falls notwendig
        or a
        ret nz
        ld e,11
        jp msgsnd

;### PRGCOP -> File kopieren
;### Ausgabe    CF=0 -> alles ok, CF=1 -> Fehler, A=Fehlercode
prgcopb equ 5*4+prgmemtab+0     ;Buffer-Bank
prgcopa equ 5*4+prgmemtab+1     ;Buffer-Adresse
prgcopl equ 5*4+prgmemtab+3     ;Buffer-Länge

prgcops db 0    ;Source-Handler
prgcopd db 0    ;Destination-Handler

prgcop  ld hl,cmdpth                ;*** Quelle öffnen
        ld a,(prgbnknum)
        db #dd:ld h,a
        push ix
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOPN
        pop ix
        jp c,prgcope
        ld (prgcops),a

        ld a,(cmdcopm)              ;*** Ziel öffnen
        sub 1
        jr c,prgcopc
        jr z,prgcop5
        ld (cmdcopm),a
prgcopc ld hl,cmdpth2 
        xor a
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILNEW
        jr c,prgcop7
        ld (prgcopd),a

prgcop5 ld a,(prgcopb)              ;*** Source lesen
        ld e,a
        ld hl,(prgcopa)
        ld bc,(cfgbuf)
        ld a,(prgcops)
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        jr c,prgcop6
        ld a,c
        or b
        jr z,prgcop2
        ld a,(prgcopb)              ;*** Destination schreiben
        ld e,a
        ld hl,(prgcopa)
        ld a,(prgcopd)
        call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOUT
        jr c,prgcop6
        dec a
        ld a,24
        scf
        jr z,prgcop6
        jr prgcop5
prgcop2 call prgcop3                ;*** Fertig
        ld a,(cmdcopm)
        or a
        ret nz
        call prgcop4
        xor a
        ret

prgcop6 push af                     ;*** Fehler
        call prgcop4                ;beides schließen
prgcop8 call prgcop3
        pop af
        ret
prgcop7 push af                     ;nur source schließen
        jr prgcop8

prgcop3 ld a,(prgcops)              ;*** Source schließen
prgcon9 call syscll
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        ret
prgcop4 ld a,(prgcopd)              ;*** Destination schließen
        jr prgcon9

prgcop0 ld bc,(cfgbuf)              ;*** Speicher für Copy-Buffer reservieren
        xor a
        ld e,1
        rst #20:dw jmp_memget
        ret c
        ld (prgcopa),hl
        ld (prgcopb),a
        ld hl,(cfgbuf)
        ld (prgcopl),hl
        ret
prgcop1 ld bc,(cfgbuf)              ;*** Speicher für Copy-Buffer freigeben
        ld hl,(prgcopa)
        ld a,(prgcopb)
        rst #20:dw jmp_memfre
        xor a
        ld (prgcopb),a
        ld l,a
        ld h,a
        ld (prgcopl),hl
        ret

prgcope push af
        ld a,(cmdcopm)
        dec a
        call z,prgcop4
        pop af
        ret


;==============================================================================
;### DATEN-TEIL ###############################################################
;==============================================================================

prgdatbeg

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #66,#66,#68,#88,#88,#88,#88,#88,#88,#66,#66,#61,#65,#51,#58,#88,#88,#88,#88,#88,#88,#56,#55,#51,#65,#51,#58,#66,#66,#66,#66,#66,#68,#56,#55,#51,#65,#51,#58,#88,#88,#88,#88,#88,#88,#56,#55,#51
db #65,#51,#58,#88,#88,#88,#88,#88,#88,#56,#55,#51,#65,#51,#58,#66,#66,#66,#66,#66,#68,#56,#55,#51,#65,#51,#58,#88,#88,#88,#88,#88,#88,#56,#55,#51,#65,#51,#58,#88,#88,#88,#88,#88,#88,#56,#55,#51
db #65,#51,#58,#66,#66,#66,#66,#66,#68,#56,#55,#51,#65,#51,#58,#88,#88,#88,#88,#88,#88,#56,#55,#51,#65,#51,#55,#55,#55,#55,#55,#55,#55,#56,#55,#51,#65,#56,#66,#66,#66,#66,#66,#66,#66,#66,#55,#51
db #65,#55,#55,#55,#55,#55,#55,#55,#55,#55,#55,#51,#65,#55,#55,#55,#55,#55,#55,#55,#55,#55,#55,#51,#65,#55,#55,#11,#11,#11,#11,#11,#11,#15,#55,#51,#65,#55,#55,#1d,#dd,#dd,#dd,#dd,#15,#66,#55,#51
db #65,#55,#55,#1d,#dd,#dd,#dd,#dd,#15,#56,#55,#51,#65,#55,#55,#1d,#11,#18,#dd,#dd,#15,#56,#55,#51,#65,#55,#55,#1d,#15,#58,#dd,#dd,#15,#56,#55,#51,#65,#55,#55,#1d,#15,#58,#dd,#dd,#15,#56,#55,#51
db #65,#55,#55,#1d,#15,#58,#dd,#dd,#15,#56,#55,#51,#65,#55,#55,#1d,#88,#88,#dd,#dd,#15,#56,#55,#51,#16,#55,#55,#1d,#dd,#dd,#dd,#dd,#15,#56,#55,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11

;### Config-Daten
cfgbeg              ;**Anfang Config**

cfglin  dw 100      ;maximale Einträge einer Liste
cfgmem  dw 2048     ;maximale Größe einer Liste
cfgatr  db 8        ;Attribut-Filter
cfgbuf  dw 4096     ;Größe des Copy-Buffers

;### Pfade
pthlen1 db 1
pthdev1 db "A:"
pthdir1 db "\"
        ds 256-2-1
pthful1 ds 256
pthmsk1 db "*.*"
        ds 9-3
pthcst1 db "*.exe"
        ds 9-5

pthlen2 db 1
pthdev2 db "A:"
pthdir2 db "\"
        ds 256-2-1
pthful2 ds 256
pthmsk2 db "*.*"
        ds 9-3
pthcst2 db "*.exe"
        ds 9-5

cfgovr  db 1        ;Flag, ob bei Überschreiben fragen

cfgend              ;**Ende Config**

;### Verschiedenes
prgmsginf1 db "SymCOMMANDER",0
prgmsginf2 db " Version 1.7 (Build "
read "..\..\..\SRC-Main\build.asm"
            db "pdt)",0
prgmsginf3 db " Copyright <c> 2025 SymbiosiS",0

prgmsgerr1  db "Disc error (Code "
prgmsgerr1a db "##):"
prgmsgerr0  db 0

prgmsgerrtb dw prgmsgerr00,prgmsgerr01,prgmsgerr02,prgmsgerr03,prgmsgerr04,prgmsgerr05,prgmsgerr06,prgmsgerr07,prgmsgerr08,prgmsgerr09
            dw prgmsgerr10,prgmsgerr11,prgmsgerr12,prgmsgerr13,prgmsgerr14,prgmsgerr15,prgmsgerr16,prgmsgerr17,prgmsgerr18,prgmsgerr19
            dw prgmsgerr20,prgmsgerr21,prgmsgerr22,prgmsgerr23,prgmsgerr24,prgmsgerr25,prgmsgerr26,prgmsgerr27,prgmsgerr28,prgmsgerr29
            dw prgmsgerr30,prgmsgerr31,prgmsgerr32
prgmsgerrmx equ 32

prgmsgerr00 db "Device does not exist",0
prgmsgerr01 db "OK",0
prgmsgerr02 db "Device not initialised",0
prgmsgerr03 db "Media is damaged",0
prgmsgerr04 db "Partition does not exist",0
prgmsgerr05 db "Unsupported media or partition",0
prgmsgerr06 db "Error while sector read/write",0
prgmsgerr07 db "Error while positioning",0
prgmsgerr08 db "Abort while volume access",0
prgmsgerr09 db "Unknown volume error",0
prgmsgerr10 db "No free filehandler",0
prgmsgerr11 db "Device does not exist",0
prgmsgerr12 db "Path does not exist",0
prgmsgerr13 db "File does not exist",0
prgmsgerr14 db "Access is forbidden",0
prgmsgerr15 db "Invalid path or filename",0
prgmsgerr16 db "Filehandler does not exist",0
prgmsgerr17 db "Device slot already occupied",0
prgmsgerr18 db "Error in file organisation",0
prgmsgerr19 db "Invalid destination name",0
prgmsgerr20 db "File/path already exist",0
prgmsgerr21 db "Wrong sub command code",0
prgmsgerr22 db "Wrong attribute",0
prgmsgerr23 db "Directory full",0
prgmsgerr24 db "Media full",0
prgmsgerr25 db "Media is write protected",0
prgmsgerr26 db "Device is not ready",0
prgmsgerr27 db "Directory is not empty",0
prgmsgerr28 db "Invalid destination device",0
prgmsgerr29 db "Not supported by file system",0
prgmsgerr30 db "Unsupported device",0
prgmsgerr31 db "File is read only",0
prgmsgerr32 db "Device channel is not available",0
prgmsgerru  db "*Undefined Error*",0
prgmsgerrs  db "*Unknown Error*",0

prgwintit   db "SymCommander 1.7",0

prgtxtok    db "Ok",0
prgtxtcnc   db "Cancel",0
prgtxtyes   db "Yes",0
prgtxtno    db "No",0

;### Menues
menicn_null         db 4,8,1:dw $+7,$+4,4:db 5: db #66,#66,#66,#66

prgwinmentx1 db "Files",0
prgwinmen1tx1 db 6,128,-1:dw menicn_fileopen    +1:db " Open",0
prgwinmen1tx2 db 6,128,-1:dw menicn_attributes  +1:db " Change Attributes...",0
prgwinmen1tx3 db 6,128,-1:dw menicn_properties  +1:db " Properties",0
prgwinmen1tx4 db 6,128,-1:dw menicn_driveinfo   +1:db " Drive information",0
prgwinmen1tx5 db 6,128,-1:dw menicn_filesplit   +1:db " Split File...",0
prgwinmen1tx6 db 6,128,-1:dw menicn_filecombine +1:db " Combine Files...",0
prgwinmen1tx7 db 6,128,-1:dw menicn_quit        +1:db " Quit",0

menicn_fileopen     db 4,8,7:dw $+7,$+4,28:db 5: db #61,#16,#66,#66, #18,#81,#16,#66, #18,#88,#77,#77, #18,#87,#22,#27, #18,#72,#22,#76, #17,#22,#27,#66, #77,#77,#76,#66
menicn_attributes   db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#61,#16, #66,#00,#01,#11, #60,#88,#11,#01, #60,#88,#11,#06, #11,#81,#18,#06, #61,#11,#18,#06, #66,#11,#00,#66
menicn_properties   db 4,8,7:dw $+7,$+4,28:db 5: db #16,#77,#17,#77, #66,#66,#66,#66, #16,#77,#77,#17, #66,#66,#66,#66, #86,#77,#71,#77, #66,#66,#66,#66, #16,#71,#77,#77
menicn_driveinfo    db 4,8,7:dw $+7,$+4,28:db 5: db #66,#77,#00,#66, #67,#77,#00,#06, #77,#77,#00,#00, #77,#77,#99,#00, #77,#79,#99,#99, #67,#99,#99,#96, #66,#99,#99,#66
menicn_filesplit    db 4,8,7:dw $+7,$+4,28:db 5: db #68,#8f,#f8,#86, #68,#ff,#ff,#86, #68,#88,#88,#86, #11,#11,#11,#11, #68,#88,#88,#86, #68,#ff,#ff,#86, #68,#8f,#f8,#86
menicn_filecombine  db 4,8,7:dw $+7,$+4,28:db 5: db #68,#99,#99,#86, #68,#89,#98,#86, #68,#88,#88,#86, #11,#11,#11,#11, #68,#88,#88,#86, #68,#89,#98,#86, #68,#99,#99,#86
menicn_quit         db 4,8,7:dw $+7,$+4,28:db 5: db #11,#16,#16,#66, #14,#46,#11,#66, #14,#11,#1e,#16, #14,#1e,#ee,#e1, #14,#11,#1e,#16, #14,#46,#11,#66, #11,#16,#16,#66

prgwinmentx2 db "Mark",0
prgwinmen2tx1 db 6,128,-1:dw menicn_selectadd   +1:db " Select Group...",0
prgwinmen2tx2 db 6,128,-1:dw menicn_selectremove+1:db " Unselect Group...",0
prgwinmen2tx3 db 6,128,-1:dw menicn_selectall   +1:db " Select All",0
prgwinmen2tx4 db 6,128,-1:dw menicn_selectnone  +1:db " Unselect All",0
prgwinmen2tx5 db 6,128,-1:dw menicn_selectinvert+1:db " Invert Selection",0
prgwinmen2tx6 db 6,128,-1:dw menicn_null        +1:db " Compare Directories",0

menicn_selectadd    db 4,8,7:dw $+7,$+4,28:db 5: db #ff,#6f,#f6,#66, #ff,#ff,#f6,#66, #6f,#ff,#66,#66, #ff,#ff,#f6,#66, #ff,#6f,#f6,#96, #66,#66,#69,#99, #66,#66,#66,#96
menicn_selectremove db 4,8,7:dw $+7,$+4,28:db 5: db #ff,#6f,#f6,#66, #ff,#ff,#f6,#66, #6f,#ff,#66,#66, #ff,#ff,#f6,#66, #ff,#6f,#f6,#66, #66,#66,#63,#33, #66,#66,#66,#66
menicn_selectall    db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #6f,#f6,#6f,#f6, #6f,#ff,#ff,#f6, #66,#ff,#ff,#66, #66,#ff,#ff,#66, #6f,#ff,#ff,#f6, #6f,#f6,#6f,#f6
menicn_selectnone   db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #68,#86,#68,#86, #68,#88,#88,#86, #66,#88,#88,#66, #66,#88,#88,#66, #68,#88,#88,#86, #68,#86,#68,#86
menicn_selectinvert db 4,8,7:dw $+7,$+4,28:db 5: db #ff,#6f,#f6,#66, #ff,#ff,#f6,#66, #6f,#ff,#66,#66, #ff,#ff,#f6,#66, #ff,#6f,#f1,#11, #66,#66,#61,#18, #66,#66,#61,#88
;     _comparedirs

prgwinmentx3 db "Commands",0
prgwinmen3tx1 db 6,128,-1:dw menicn_copy        +1:db " Copy Files...",0
prgwinmen3tx2 db 6,128,-1:dw menicn_move        +1:db " Move Files...",0
prgwinmen3tx3 db 6,128,-1:dw menicn_delete      +1:db " Delete Files",0
prgwinmen3tx4 db 6,128,-1:dw menicn_folder      +1:db " New Folder...",0
prgwinmen3tx5 db 6,128,-1:dw menicn_fileview    +1:db " View File",0
prgwinmen3tx6 db 6,128,-1:dw menicn_fileedit    +1:db " Edit File",0
prgwinmen3tx7 db 6,128,-1:dw menicn_find        +1:db " Search...",0
prgwinmen3tx8 db 6,128,-1:dw menicn_dirswap     +1:db " Source <-> Target",0
prgwinmen3tx9 db 6,128,-1:dw menicn_dirsame     +1:db " Target = Source",0

menicn_copy         db 4,8,7:dw $+7,$+4,28:db 5: db #55,#55,#56,#66, #58,#88,#56,#66, #58,#88,#57,#77, #58,#88,#50,#07, #55,#55,#50,#07, #66,#67,#00,#07, #66,#67,#77,#77
menicn_move         db 4,8,7:dw $+7,$+4,28:db 5: db #66,#33,#36,#66, #33,#22,#23,#36, #32,#22,#22,#36, #32,#55,#55,#55, #33,#58,#88,#85, #66,#58,#88,#85, #66,#55,#55,#55
menicn_delete       db 4,8,7:dw $+7,$+4,28:db 5: db #ff,#66,#66,#ff, #6f,#f6,#6f,#f6, #66,#ff,#ff,#66, #66,#6f,#f6,#66, #66,#ff,#ff,#66, #6f,#f6,#6f,#f6, #ff,#66,#66,#ff
menicn_folder       db 4,8,7:dw $+7,$+4,28:db 5: db #6d,#dd,#66,#66, #d0,#00,#dd,#d6, #d0,#07,#00,#01, #d0,#77,#70,#01, #d0,#07,#00,#01, #d0,#00,#00,#01, #61,#11,#11,#16
menicn_fileview     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#77,#77,#76, #66,#78,#88,#77, #66,#11,#18,#87, #66,#18,#18,#87, #66,#11,#18,#87, #61,#78,#88,#87, #16,#77,#77,#77
menicn_fileedit     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#16, #ff,#ff,#81,#01, #88,#88,#10,#d6, #ff,#81,#0d,#86, #88,#30,#d8,#86, #f8,#33,#8f,#f6, #88,#88,#88,#86
menicn_find         db 4,8,7:dw $+7,$+4,28:db 5: db #66,#16,#61,#66, #61,#71,#17,#16, #61,#71,#17,#16, #17,#71,#17,#71, #18,#16,#61,#81, #17,#16,#61,#71, #11,#16,#61,#11
menicn_dirswap      db 4,8,7:dw $+7,$+4,28:db 5: db #61,#11,#11,#16, #17,#77,#76,#71, #18,#8f,#88,#81, #18,#f8,#8f,#81, #18,#f8,#8f,#81, #18,#88,#f8,#81, #61,#11,#11,#16
menicn_dirsame      db 4,8,7:dw $+7,$+4,28:db 5: db #61,#11,#11,#16, #17,#77,#76,#71, #18,#99,#99,#81, #18,#88,#88,#81, #18,#99,#99,#81, #18,#88,#88,#81, #61,#11,#11,#16

prgwinmentx4 db "Show",0
prgwinmen4tx1  db 6,128,-1:dw menicn_showall     +1:db  " All Files",0
prgwinmen4tx2  db 6,128,-1:dw menicn_null        +1:db  " xxxxxxxx",0
prgwinmen4tx3  db 6,128,-1:dw menicn_showcustom  +1:db  " Custom...",0
prgwinmen4tx4  db 6,128,-1:dw menicn_sortname    +1:db  " Sort By Name",0
prgwinmen4tx5  db 6,128,-1:dw menicn_sortsize    +1:db  " Sort By Size",0
prgwinmen4tx6  db 6,128,-1:dw menicn_datetime    +1:db  " Sort By Date",0
prgwinmen4tx7  db 6,128,-1:dw menicn_sortreverse +1:db  " Reversed Order",0
prgwinmen4tx8  db 6,128,-1:dw menicn_refresh     +1:db  " Reread Source",0

menicn_showall      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#69,#96,#66, #99,#69,#96,#99, #99,#99,#99,#99, #66,#99,#99,#66, #69,#99,#99,#96, #99,#96,#69,#99, #99,#66,#66,#99
menicn_showcustom   db 4,8,7:dw $+7,$+4,28:db 5: db #6f,#ff,#ff,#f6, #ff,#f6,#6f,#ff, #66,#66,#6f,#ff, #66,#6f,#ff,#f6, #66,#ff,#f6,#66, #66,#66,#66,#66, #66,#ff,#f6,#66
menicn_sortname     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #61,#66,#66,#55, #16,#17,#65,#66, #11,#17,#65,#66, #16,#17,#77,#66, #16,#17,#67,#55, #66,#67,#77,#66
menicn_sortsize     db 4,8,7:dw $+7,$+4,28:db 5: db #65,#66,#66,#66, #55,#66,#61,#16, #65,#77,#66,#61, #65,#66,#76,#11, #65,#67,#66,#61, #66,#76,#61,#16, #66,#77,#76,#66
menicn_datetime     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#77,#77,#66, #67,#8a,#18,#76, #78,#88,#18,#87, #7a,#81,#88,#a7, #78,#18,#88,#87, #67,#88,#a8,#76, #66,#77,#77,#66
menicn_sortreverse  db 4,8,7:dw $+7,$+4,28:db 5: db #11,#16,#66,#66, #66,#16,#66,#66, #61,#66,#67,#76, #16,#66,#76,#67, #11,#16,#77,#77, #66,#66,#76,#67, #66,#66,#76,#67
menicn_refresh      db 4,8,7:dw $+7,$+4,28:db 5: db #00,#99,#90,#00, #09,#00,#09,#09, #00,#00,#00,#99, #99,#90,#09,#99, #99,#00,#00,#00, #90,#90,#00,#90, #00,#09,#99,#00

prgwinmentx5 db "Configuration",0
prgwinmen5tx1 db 6,128,-1:dw menicn_settings    +1:db " Options...",0
prgwinmen5tx2 db 6,128,-1:dw menicn_filesave    +1:db " Save Settings",0

menicn_settings     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#6c,#66,#66, #6c,#6c,#6c,#66, #6f,#cd,#cf,#66, #cc,#c1,#cc,#c6, #ff,#cc,#cf,#f6, #6c,#fc,#fc,#66, #6f,#6c,#6f,#66
menicn_filesave     db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#11, #1f,#ee,#ee,#f1, #1f,#ee,#ee,#f1, #1f,#ff,#ff,#f1, #1f,#11,#c1,#f1, #1f,#11,#c1,#f1, #61,#11,#11,#11

prgwinmentx6 db "?",0
prgwinmen6tx1 db 6,128,-1:dw menicn_help        +1:db " Help topics",0
prgwinmen6tx2 db 6,128,-1:dw menicn_about       +1:db " About SymCommander...",0

menicn_help         db 4,8,7:dw $+7,$+4,28:db 5: db #66,#1f,#f1,#66, #61,#fc,#cf,#16, #1f,#ff,#fc,#f1, #ff,#fc,#cc,#f1, #ff,#ff,#ff,#18, #1f,#cf,#f1,#81, #61,#ff,#18,#16
menicn_about        db 4,8,7:dw $+7,$+4,28:db 5: db #66,#10,#07,#66, #66,#10,#07,#66, #66,#66,#66,#66, #61,#00,#07,#66, #66,#10,#07,#66, #66,#10,#07,#66, #61,#00,#00,#76

;### Haupt-Fenster-Texte
prgobjtxt1  db "View",0
prgobjtxt2  db "Edit",0
prgobjtxt3  db "Copy",0
prgobjtxt4  db "Move",0
prgobjtxt5  db "Folder",0
prgobjtxt6  db "Delete",0
prgobjtxt7  db ">",0
lstanz1     dw 0
prgobjtxt8  ds 50
lstanz2     dw 0
prgobjtxt9  ds 50
prgtabdev   ds 8*5
prgobjtxta  db 0

;### Dialog-Fenster-Texte
filtyptxt   db "Enter file type (e.g. *.exe, *.txt)",0
cmdcoptxt   db "Copy "
cmdcoptxt1  db "??? file(s) to",0
cmdcoptxt2  db " file(s) to",0
cmdmovtxt   db "Move "
cmdmovtxt1  db "??? file(s) to",0
cmdrentxt   db "Rename file/directory to",0
cmdfoltxt   db "New directory",0
splitttxt1  db "Split the file to directory:",0
splitttxt2  db "KBytes per file:",0
combintxt   db "Combine selection to file:",0
confrmtxt1  db "Do you really want to delete the "
confrmtxt1a db "???",0
confrmtxt2  db "selected file(s)/directorie(s)?",0

drvprptit   db "Drive information",0
drvprptxt0  db "Drive #",0
drvprptxt1  db "Type",0
drvprptxt20 db "[unknown]",0
drvprptxt2a db "Floppy Single Side",0
drvprptxt2b db "Floppy Double Side",0
drvprptxt2c db "IDE device",0
drvprptxt2d db "SD/MMC card",0
drvprptxt2e db "USB device",0
drvprptxt3  db "File system",0
drvprptxt4a db "Amsdos Data",0
drvprptxt4b db "Amsdos System",0
drvprptxt4c db "PCW",0
drvprptxt4d db "FAT 12",0
drvprptxt4e db "FAT 16",0
drvprptxt4f db "FAT 32",0
drvprptxt5  db "Used",0
drvprptxt6  db "########### KBytes",0
drvprptxt60 db "[examining...]",0
drvprptxt61 db "[Error]",0
drvprptxt62 db " KBytes",0
drvprptxt7  db "Free",0
drvprptxt8  db "########### KBytes",0
drvprptxt9  db "Capacity",0
drvprptxta  db "########### KBytes",0
drvprptxtb0 db "(not removable)",0
drvprptxtb1 db "(removable)",0

;### Property-Fenster-Texte
propertit   db "Properties",0
propertxt0  db "Name",0
propertxt1  db "File type",0
propertxt2  db "Open with",0
propertxt3  db "Location",0
propertxt4  db "Size",0
propertxt5  db "Created",0
propertxt6  db "Modified",0
propertxt7  db "Attributes",0
propertxt8  ds 33

properdtm1  db "07.08.2005, 18:09:34",0
properdtm2  db "07.08.2005, 18:09:34",0

propertxta  db "Read only",0
propertxtb  db "Hidden",0
propertxtc  db "System",0
propertxtd  db "Archive",0

propertxte  db "EXE-file",0
propertxtf  db "########### Bytes",0,0
propertxtg  db " Bytes",0
propertxth  db "directory",0

;### Attributes-Fenster-Texte
attribtit   db "Change attributes",0
attribtxt0  db "Date and time",0
attribtxt1  db "Attributes",0
attribtxt2  db "Change date/time:",0
attribtxt3  db "set",0
attribtxt4  db "reset",0
attribtxt5  db "unchng.",0
attribtxt6  db "Date:",0
attribtxt7  db "Time:",0
attribtxt8  db "Current",0

;### Config-Fenster-Texte
configtit   db "Configuration",0
configtxt0  db "Memory usage",0
configtxt1  db "Max. list entries",0
configtxt2  db "List buffer size",0
configtxt3  db "Copy buffer size",0
configtxt4  db "bytes",0
configtxt5  db "Miscellaneous",0
configtxt6  db "files",0
configtxt7  db "Show hidden/system files",0
configtxt8  db "Ask before overwriting files",0

;### Copy/Move-Fenster-Texte
copmovtxt1a db "Copy:",0
copmovtxt1b db "Move:"
copmovtxt0  db 0
copmovtxt2  db "From:",0
copmovtxt3  db "To:",0

;### Overwrite-Fenster-Texte
ovtwrttxt0  db "...already exist. What to do?",0
ovtwrttxt1  db "Overwrite",0
ovtwrttxt2  db "Overwr.all",0
ovtwrttxt3  db "Skip",0
ovtwrttxt4  db "Skip all",0


;==============================================================================
;### TRANSFER-TEIL ############################################################
;==============================================================================

prgtrnbeg
;### PRGPRZS -> Stack für Programm-Prozess
        ds 128
prgstk  ds 6*2
        dw prgprz
prgprzn db 0
prgmsgb ds 14

;### VERSCHIEDENES ############################################################

cfgdev  ds 8*16

;### DIALOG INPUT FENSTER #####################################################

diainpwin   dw #1401,4+16,079,062,160,50,0,0,160,50,160,50,160,50,0,prgwintit,0,0,diainpgrp,0,0:ds 136+14
diainpgrp   db 5,0:dw diainpdat,0,0,256*5+4,0,0,3
diainpdat
dw      00,         0,2,        0,0,1000,1000,0         ;00=Hintergrund
dw      00,255*256+ 1,diainpdsc, 05,05,150, 8,0         ;01=Beschreibung
dw      00,255*256+32,diainpinp, 05,16,150,12,0         ;02=Eingabe
dw diainpo,255*256+16,prgtxtok,  57
diainpdat1                       dw 33, 48,12,0         ;03="Ok"    -Button
dw diainpc,255*256+16,prgtxtcnc,107
diainpdat2                       dw 33, 48,12,0         ;04="Cancel"-Button
dw      00,255*256+ 1,splittdsc ,05,33, 65, 8,0         ;05=Beschreibung 2
dw      00,255*256+32,splittinp ,70,31, 40,12,0         ;06=Eingabe Bytes

diainpdsc   dw filtyptxt,2+4
diainpinp   dw diainpbuf,0,0,0,0,0,0
diainpbuf   ds 256+10+65

;### BESTÄTIGUNGS FENSTER #####################################################

confrmwin   dw #1401,4+16,079,062,160,46,0,0,160,46,160,46,160,46,0,prgwintit,0,0,confrmgrp,0,0:ds 136+14
confrmgrp   db 5,0:dw confrmdat,0,0,256*5+4,0,0,00
confrmdat
dw      00,         0,2,        0,0,1000,1000,0         ;00=Hintergrund
dw      00,255*256+ 1,confrmdsc1,05,05,150, 8,0         ;01=Beschreibung1
dw      00,255*256+ 1,confrmdsc2,05,15,150, 8,0         ;02=Beschreibung2
dw cmddel0,255*256+16,prgtxtyes, 31,29, 48,12,0         ;03="Yes"-Button
dw diainpc,255*256+16,prgtxtno , 81,29, 48,12,0         ;04="No" -Button

confrmdsc1  dw confrmtxt1,2+4
confrmdsc2  dw confrmtxt2,2+4

;### SPLIT FILE FENSTER #######################################################

splittdsc   dw splitttxt2,2+4
splittinp   dw splittbuf,0,0,0,3,5,0
splittbuf   db "178",0,0,0

;### OVERWRITE FENSTER ########################################################

ovrwrtwin   dw #1401,4+16,079,060,160,55,0,0,160,55,160,55,160,55,0,prgwintit,0,0,ovrwrtgrp,0,0:ds 136+14
ovrwrtgrp   db 8,0:dw ovrwrtdat,0,0,256*8+4,0,0,04
ovrwrtdat
dw      00,         0,2,         0,0,1000,1000,0        ;00=Hintergrund
dw      00,255*256+ 1,ovrwrtdsc1, 05,03,150, 8,0        ;01=Beschreibung
dw      00,255*256+ 1,ovrwrtdsc2, 05,13, 25, 8,0        ;02=Beschreibung
dw cmdcov1,255*256+16,ovtwrttxt1, 05,23, 48,12,0        ;03="Overwrite"    -Button
dw cmdcov2,255*256+16,ovtwrttxt2, 57,23, 48,12,0        ;04="Overwrite All"-Button
dw cmdcov3,255*256+16,ovtwrttxt3, 05,39, 48,12,0        ;05="Skip"         -Button
dw cmdcov4,255*256+16,ovtwrttxt4, 57,39, 48,12,0        ;06="Skip All"     -Button
dw diainpc,255*256+16,prgtxtcnc, 109,39, 46,12,0        ;07="Cancel"       -Button

ovrwrtdsc1  dw cmdpth2,2+4+128
ovrwrtdsc2  dw ovtwrttxt0,2+4

;### COPY/MOVE FENSTER ########################################################

copmovwin   dw #1401,4+16,079,060,160,64,0,0,160,64,160,64,160,64,0,prgwintit,0,0,copmovgrp,0,0:ds 136+14
copmovgrp   db 8,0:dw copmovdat,0,0,256*8+0,0,0,00
copmovdat
dw      00,         0,2,         0,0,1000,1000,0        ;00=Hintergrund
dw      00,255*256+ 1,copmovdsc1, 05,03,150, 8,0        ;01=Beschreibung
dw      00,255*256+ 1,copmovdsc2, 05,13, 25, 8,0        ;02=Beschreibung
dw      00,255*256+ 1,copmovdsc3, 05,23, 25, 8,0        ;03=Beschreibung
dw      00,255*256+ 1,copmovsrc,  30,13,125, 8,0        ;04=Quelle
dw      00,255*256+ 1,copmovdst,  30,23,125, 8,0        ;05=Ziel
dw      00,255*256+ 4
copmovdat1 dw     256*000+1+4+48, 05,34,150,10,0        ;06=Fortschrittsbalken
dw cmdcnc ,255*256+16,prgtxtcnc,  56,48, 48,12,0        ;07="Cancel"-Button

copmovdsc1  dw copmovtxt1a,2+4+512
copmovdsc2  dw copmovtxt2,2+4
copmovdsc3  dw copmovtxt3,2+4
copmovsrc   dw copmovtxt0,2+4+128
copmovdst   dw copmovtxt0,2+4+128

;### DRIVE-INFO FENSTER #######################################################

drvprpwin   dw #1401,4+16,079,041,160,101,0,0,160,101,160,101,160,101,0,drvprptit,0,0,drvprpgrp,0,0:ds 136+14
drvprpgrp   db 17,0:dw drvprpdat,0,0,256*17+17,0,0,17
drvprpdat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 1,drvprpdsc0, 05, 07, 55, 8,0       ;01=Beschreibung "Drive X"
dw      00,255*256+ 1,drvprpdscb, 61, 07, 94, 8,0       ;02=Angabe       "Removable"
dw      00,         0,1,          05, 20,150, 1,0       ;03=Trennlinie
dw      00,255*256+ 1,drvprpdsc1, 05, 24, 55, 8,0       ;04=Beschreibung "Type"
dw      00,255*256+ 1,drvprpdsc2, 61, 24, 94, 8,0       ;05=Angabe       "Type"
dw      00,255*256+ 1,drvprpdsc3, 05, 34, 55, 8,0       ;06=Beschreibung "Filesystem"
dw      00,255*256+ 1,drvprpdsc4, 61, 34, 94, 8,0       ;07=Angabe       "Filesystem"
dw      00,         0,1,          05, 45,150, 1,0       ;08=Trennlinie
dw      00,255*256+ 1,drvprpdsc5, 05, 49, 55, 8,0       ;09=Beschreibung "Belegt"
dw      00,255*256+ 1,drvprpdsc7, 05, 59, 55, 8,0       ;10=Beschreibung "Frei"
dw      00,255*256+ 1,drvprpdsc6, 61, 49, 94, 8,0       ;11=Angabe       "Belegt"
dw      00,255*256+ 1,drvprpdsc8, 61, 59, 94, 8,0       ;12=Angabe       "Frei"
dw      00,255*256+ 1,drvprpdsc9, 05, 69, 55, 8,0       ;13=Beschreibung "Gesamt"
dw      00,255*256+ 1,drvprpdsca, 61, 69, 94, 8,0       ;14=Angabe       "Gesamt"
dw      00,         0,1,          05, 80,150, 1,0       ;15=Trennlinie
dw diainpc,255*256+16,prgtxtok,   56, 84, 48,12,0       ;16="Ok"    -Button

drvprpdsc0  dw drvprptxt0,2+4
drvprpdsc1  dw drvprptxt1,2+4
drvprpdsc2  dw drvprptxt20,2+4
drvprpdsc3  dw drvprptxt3,2+4
drvprpdsc4  dw drvprptxt20,2+4
drvprpdsc5  dw drvprptxt5,2+4
drvprpdsc6  dw drvprptxt60,2+4+128
drvprpdsc7  dw drvprptxt7,2+4
drvprpdsc8  dw drvprptxt60,2+4+128
drvprpdsc9  dw drvprptxt9,2+4
drvprpdsca  dw drvprptxta,2+4
drvprpdscb  dw drvprptxtb0,2+4

;### PROPERTY FENSTER #########################################################

properwin   dw #1401,4+16,079,011,160,161,0,0,160,161,160,161,160,161,0,propertit,0,0,propergrp,0,0:ds 136+14
propergrp   db 27,0:dw properdat,0,0,256*27+26,0,0,3
properdat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 1,properdsc0, 05, 07, 55, 8,0       ;01=Beschreibung "Name"
dw      00,255*256+32,diainpinp,  51, 05,104,12,0       ;02=Eingabe "Name"
dw      00,         0,1,          05, 20,150, 1,0       ;03=Trennlinie
dw      00,255*256+ 1,properdsc1, 05, 24, 55, 8,0       ;04=Beschreibung "File type"
dw      00,255*256+ 1,propercon1, 51, 24,104, 8,0       ;05=Angabe "File type"
dw      00,255*256+ 1,properdsc2, 05, 34, 55, 8,0       ;06=Beschreibung "Open with"
dw      00,255*256+ 1,properdsc8, 51, 34,104, 8,0       ;07=Beschreibung "not defined"
dw      00,         0,1,          05, 45,150, 1,0       ;08=Trennlinie
dw      00,255*256+ 1,properdsc3, 05, 49, 55, 8,0       ;09=Beschreibung "Location"
dw      00,255*256+ 1,propercon3, 51, 49,104, 8,0       ;10=Angabe "Location"
dw      00,255*256+ 1,properdsc4, 05, 59, 55, 8,0       ;11=Beschreibung "Size"
dw      00,255*256+ 1,propercon4, 51, 59,104, 8,0       ;12=Angabe "Size"
dw      00,         0,1,          05, 70,150, 1,0       ;13=Trennlinie
dw      00,255*256+ 1,properdsc5, 05, 74, 55, 8,0       ;14=Beschreibung "Created"
dw      00,255*256+ 1,propercon5, 51, 74,104, 8,0       ;15=Angabe "Size"
dw      00,255*256+ 1,properdsc6, 05, 84, 55, 8,0       ;16=Beschreibung "Modified"
dw      00,255*256+ 1,propercon6, 51, 84,104, 8,0       ;17=Angabe "Size"
dw      00,         0,1,          05, 95,150, 1,0       ;18=Trennlinie
dw      00,255*256+ 1,properdsc7, 05, 99, 55, 8,0       ;19=Beschreibung "Attributes"
dw      00,255*256+17,properchk1, 51, 99,104, 8,0       ;20=Attribut "ReadOnly"
dw      00,255*256+17,properchk2, 51,109,104, 8,0       ;21=Attribut "Hidden"
dw      00,255*256+17,properchk3, 51,119,104, 8,0       ;22=Attribut "System"
dw      00,255*256+17,properchk4, 51,129,104, 8,0       ;23=Attribut "Archiv"
dw      00,         0,1,          05,140,150, 1,0       ;24=Trennlinie
dw filprp0,255*256+16,prgtxtok,   57,144, 48,12,0       ;25="Ok"    -Button
dw diainpc,255*256+16,prgtxtcnc, 107,144, 48,12,0       ;26="Cancel"-Button

properdsc0  dw propertxt0,2+4
properdsc1  dw propertxt1,2+4
properdsc2  dw propertxt2,2+4
properdsc3  dw propertxt3,2+4
properdsc4  dw propertxt4,2+4
properdsc5  dw propertxt5,2+4
properdsc6  dw propertxt6,2+4
properdsc7  dw propertxt7,2+4
properdsc8  dw propertxt8,2+4

propercon1  dw propertxte,2+4
propercon3  dw 00,2+4
propercon4  dw propertxtf,2+4
propercon5  dw properdtm1,2+4
propercon6  dw properdtm2,2+4

properchk1  dw properatr1,propertxta,2+4
properchk2  dw properatr2,propertxtb,2+4
properchk3  dw properatr3,propertxtc,2+4
properchk4  dw properatr4,propertxtd,2+4
properatr1  db 0
properatr2  db 0
properatr3  db 0
properatr4  db 0

;### ATTRIBUTES FENSTER #######################################################

attribwin   dw #1401,4+16,079,035,160,114,0,0,160,114,160,114,160,114,0,attribtit,0,0,attribgrp,0,0:ds 136+14
attribgrp   db 27,0:dw attribdat,0,0,256*27+26,0,0,00
attribdat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 3,attribdsc1, 00, 01,160,56,0       ;01=Rahmen "Attribute"
dw      00,255*256+ 1,attribdsc3, 08, 11, 44, 8,0       ;02=Beschreibung "ReadOnly"
dw      00,255*256+18,attribrad1a,54, 11, 24, 8,0       ;03=Attribut "ReadOnly set"
dw      00,255*256+18,attribrad1b,78, 11, 33, 8,0       ;04=Attribut "ReadOnly reset"
dw      00,255*256+18,attribrad1c,111,11, 43, 8,0       ;05=Attribut "ReadOnly unchanged"
dw      00,255*256+ 1,attribdsc4, 08, 21, 44, 8,0       ;06=Beschreibung "Hidden"
dw      00,255*256+18,attribrad2a,54, 21, 24, 8,0       ;07=Attribut "Hidden set"
dw      00,255*256+18,attribrad2b,78, 21, 33, 8,0       ;08=Attribut "Hidden reset"
dw      00,255*256+18,attribrad2c,111,21, 43, 8,0       ;09=Attribut "Hidden unchanged"
dw      00,255*256+ 1,attribdsc5, 08, 31, 44, 8,0       ;10=Beschreibung "System"
dw      00,255*256+18,attribrad3a,54, 31, 24, 8,0       ;11=Attribut "System set"
dw      00,255*256+18,attribrad3b,78, 31, 33, 8,0       ;12=Attribut "System reset"
dw      00,255*256+18,attribrad3c,111,31, 43, 8,0       ;13=Attribut "System unchanged"
dw      00,255*256+ 1,attribdsc6, 08, 41, 44, 8,0       ;14=Beschreibung "Archive"
dw      00,255*256+18,attribrad4a,54, 41, 24, 8,0       ;15=Attribut "Archive set"
dw      00,255*256+18,attribrad4b,78, 41, 33, 8,0       ;16=Attribut "Archive reset"
dw      00,255*256+18,attribrad4c,111,41, 43, 8,0       ;17=Attribut "Archive unchanged"
dw      00,255*256+ 3,attribdsc2, 00, 56,160,42,0       ;18=Rahmen "Datum/Zeit"
dw      00,255*256+17,attribchk1, 08, 66, 96, 8,0       ;19=Checkbox "Zeit ändern"
dw filatr3,255*256+16,attribtxt8,104, 64, 48,12,0       ;20="Current"-Button
dw      00,255*256+ 1,attribdsc7 ,08, 80, 22, 8,0       ;21=Beschreibung "Datum"
dw      00,255*256+32,attribinp1, 30, 78, 50,12,0       ;22=Eingabe "Datum"
dw      00,255*256+ 1,attribdsc8 ,86, 80, 23, 8,0       ;23=Beschreibung "Zeit"
dw      00,255*256+32,attribinp2,109, 78, 28,12,0       ;24=Eingabe "Zeit"
dw filatr0,255*256+16,prgtxtok,   59, 98, 48,12,0       ;25="Ok"    -Button
dw diainpc,255*256+16,prgtxtcnc, 109, 98, 48,12,0       ;26="Cancel"-Button

attribdsc1  dw attribtxt1,2+4
attribdsc2  dw attribtxt0,2+4
attribdsc3  dw propertxta,2+4
attribdsc4  dw propertxtb,2+4
attribdsc5  dw propertxtc,2+4
attribdsc6  dw propertxtd,2+4
attribdsc7  dw attribtxt6,2+4
attribdsc8  dw attribtxt7,2+4

attribinp1  dw attribbuf1,0,0,0,10,10,0
attribbuf1  db "01.01.2007",0
attribinp2  dw attribbuf2,0,0,0,5,5,0
attribbuf2  db "23:59",0

attribtim   db 0
attribchk1  dw attribtim,attribtxt2,2+4

attribrad1  db 2
attribrad2  db 2
attribrad3  db 2
attribrad4  db 2

attribrad1p ds 4
attribrad1a dw attribrad1,attribtxt3,2+4+256,attribrad1p
attribrad1b dw attribrad1,attribtxt4,2+4+000,attribrad1p
attribrad1c dw attribrad1,attribtxt5,2+4+512,attribrad1p
attribrad2p ds 4
attribrad2a dw attribrad2,attribtxt3,2+4+256,attribrad2p
attribrad2b dw attribrad2,attribtxt4,2+4+000,attribrad2p
attribrad2c dw attribrad2,attribtxt5,2+4+512,attribrad2p
attribrad3p ds 4
attribrad3a dw attribrad3,attribtxt3,2+4+256,attribrad3p
attribrad3b dw attribrad3,attribtxt4,2+4+000,attribrad3p
attribrad3c dw attribrad3,attribtxt5,2+4+512,attribrad3p
attribrad4p ds 4
attribrad4a dw attribrad4,attribtxt3,2+4+256,attribrad4p
attribrad4b dw attribrad4,attribtxt4,2+4+000,attribrad4p
attribrad4c dw attribrad4,attribtxt5,2+4+512,attribrad4p

;### CONFIG FENSTER ###########################################################

configwin   dw #1401,4+16,079,035,160,109,0,0,160,109,160,109,160,109,0,configtit,0,0,configgrp,0,0:ds 136+14
configgrp   db 16,0:dw configdat,0,0,256*16+15,0,0,04
configdat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 3,configdsc0, 00, 01,160,57,0       ;01=Rahmen "Memory"
dw      00,255*256+ 1,configdsc1, 08, 13, 54, 8,0       ;02=Beschreibung "Filelist entries"
dw      00,255*256+32,configinp1, 78, 11, 34,12,0       ;03=Eingabe "Filelist entries"
dw      00,255*256+ 1,configdsc6,114, 13, 30, 8,0       ;04=Beschreibung "files"
dw      00,255*256+ 1,configdsc2, 08, 27, 54, 8,0       ;05=Beschreibung "Filelist buffer"
dw      00,255*256+32,configinp2, 78, 25, 34,12,0       ;06=Eingabe "Filelist buffer"
dw      00,255*256+ 1,configdsc4,114, 27, 30, 8,0       ;07=Beschreibung "bytes"
dw      00,255*256+ 1,configdsc3, 08, 41, 54, 8,0       ;08=Beschreibung "Filecopy buffer"
dw      00,255*256+32,configinp3, 78, 39, 34,12,0       ;09=Eingabe "Filecopy buffer"
dw      00,255*256+ 1,configdsc4,114, 41, 30, 8,0       ;10=Beschreibung "bytes"
dw      00,255*256+ 3,configdsc5, 00, 57,160,36,0       ;11=Rahmen "Misc"
dw      00,255*256+17,configchk1, 08, 67,144, 8,0       ;12=Checkbox "Show hidden/system files"
dw      00,255*256+17,configchk2, 08, 77,144, 8,0       ;13=Checkbox "Ask before overwriting files"
dw cfgset1,255*256+16,prgtxtok,   59, 93, 48,12,0       ;14="Ok"    -Button
dw diainpc,255*256+16,prgtxtcnc, 109, 93, 48,12,0       ;15="Cancel"-Button

configdsc0  dw configtxt0,2+4
configdsc1  dw configtxt1,2+4
configdsc2  dw configtxt2,2+4
configdsc3  dw configtxt3,2+4
configdsc4  dw configtxt4,2+4
configdsc5  dw configtxt5,2+4
configdsc6  dw configtxt6,2+4

configinp1  dw configbuf1,0,0,0,0,4,0
configbuf1  ds 6
configinp2  dw configbuf2,0,0,0,0,5,0
configbuf2  ds 6
configinp3  dw configbuf3,0,0,0,0,5,0
configbuf3  ds 6

configatr   db 1
configchk1  dw configatr,configtxt7,2+4
configovr   db 0
configchk2  dw configovr,configtxt8,2+4

;### HAUPT-FENSTER ############################################################

prgwindat dw #3702,3,05,05,300,150,0,0,300,150,200,100,10000,10000,prgicnsml,prgwintit,0,prgwinmen,prgwingrp,0,0:ds 136+14

prgwinmen  dw 6, 1+4,prgwinmentx1,prgwinmen1,0, 1+4,prgwinmentx2,prgwinmen2,0, 1+4,prgwinmentx3,prgwinmen3,0, 1+4,prgwinmentx4,prgwinmen4,0, 1+4,prgwinmentx5,prgwinmen5,0, 1+4,prgwinmentx6,prgwinmen6,0
prgwinmen1 dw 9, 17,prgwinmen1tx1,filopn,0,  17,prgwinmen1tx2,filatr,0, 17,prgwinmen1tx3,filprp,0, 17,prgwinmen1tx4,fildrv,0, 1+8,#0000,0,0, 17,prgwinmen1tx5,filspl,0,     17,prgwinmen1tx6,filcmb,0,1+8,#0000,0,0, 17,prgwinmen1tx7,prgend,0
prgwinmen2 dw 7, 17,prgwinmen2tx1,mrksel,0,  17,prgwinmen2tx2,mrkdes,0, 17,prgwinmen2tx3,mrkall,0, 17,prgwinmen2tx4,mrknon,0, 17,prgwinmen2tx5,mrkinv,0, 1+8,#0000,0,0,     16,prgwinmen2tx6,mrkcmp,0
prgwinmen3 dw 11,17,prgwinmen3tx1,cmdcop,0,  17,prgwinmen3tx2,cmdmov,0, 17,prgwinmen3tx3,cmddel,0, 17,prgwinmen3tx4,cmdfol,0, 1+8,#0000,0,0, 16,prgwinmen3tx5,prgprz0,0,    16,prgwinmen3tx6,prgprz0,0, 16,prgwinmen3tx7,prgprz0,0, 1+8,#0000,0,0
           dw    17,prgwinmen3tx8,cmdswp,0,  17,prgwinmen3tx9,cmdequ,0
prgwinmen4 dw 10,17+2,prgwinmen4tx1,shwall,0,17,prgwinmen4tx2,shwdef,0, 17,prgwinmen4tx3,shwcst,0, 1+8,#0000,0,0
           dw    17+2,prgwinmen4tx4,shwnam,0,17,prgwinmen4tx5,shwsiz,0, 17,prgwinmen4tx6,shwdat,0, 1+8,#0000,0,0, 17,prgwinmen4tx7,shwrev,0, 17,prgwinmen4tx8,shwref,0
prgwinmen5 dw 2, 17,prgwinmen5tx1,cfgset,0,  17,prgwinmen5tx2,cfgsav,0
prgwinmen6 dw 3, 17,prgwinmen6tx1,hlpopn,0, 1+8,0,0,0, 17,prgwinmen6tx2,prginf,0

prgwingrp db 22,0:dw prgwinobj,prgwinclc,0,0,0,0,6
prgwinobj
dw     00,255*256+00,2         ,0,0,0,0,0   ;00=Hintergrund
dw ctldr1,255*256+42,prgobjdeva,0,0,0,0,0   ;01=Auswahl Laufwerk 1
dw ctldr2,255*256+42,prgobjdevb,0,0,0,0,0   ;02=Auswahl Laufwerk 2
dw lstfoca,255*256+1,prgobjdsc1,0,0,0,0,0   ;03=Text Pfad 1
dw lstfocb,255*256+1,prgobjdsc2,0,0,0,0,0   ;04=Text Pfad 2
dw lstclka,255*256+43
prgwinobj1        dw 00        ,0,0,0,0,0   ;05=Liste 1
dw lstclkb,255*256+43
prgwinobj2        dw 00        ,0,0,0,0,0   ;06=Liste 2
dw     00,255*256+02,3+4       ,0,0,0,0,0   ;07=Rahmen Ausgewählt 1
dw     00,255*256+02,3+4       ,0,0,0,0,0   ;08=Rahmen Ausgewählt 2
dw     00,255*256+01,prgobjdsc3,0,0,0,0,0   ;09=Text Ausgewählt 1
dw     00,255*256+01,prgobjdsc4,0,0,0,0,0   ;10=Text Ausgewählt 2
dw     00,255*256+01,prgobjdsc5,0,0,0,0,0   ;11=Text Pfad
dw     00,255*256+01,prgobjdsc6,0,0,0,0,0   ;12=Text ">" hinter Pfad
dw     00,255*256+16,prgobjtxt1,0,0,0,0,0   ;13=Button "View"
dw     00,255*256+16,prgobjtxt2,0,0,0,0,0   ;14=Button "Edit"
dw cmdcop,255*256+16,prgobjtxt3,0,0,0,0,0   ;15=Button "Copy"
dw cmdmov,255*256+16,prgobjtxt4,0,0,0,0,0   ;16=Button "Move"
dw cmdfol,255*256+16,prgobjtxt5,0,0,0,0,0   ;17=Button "Folder"
dw cmddel,255*256+16,prgobjtxt6,0,0,0,0,0   ;18=Button "Delete"
dw     00,255*256+32,prgobjinp1,0,0,0,0,0   ;19=Eingabe Commando
dw lstfoca,255*256+18,prgobjrada,0,0,0,0,0  ;20=Radio Focus Liste 1
dw lstfocb,255*256+18,prgobjradb,0,0,0,0,0  ;21=Radio Focus Liste 2

prgwinclc
dw 0,0,0,0,10000,0,10000,0
dw  10,      0,  1,  0, 27,      0, 10,  0
dw   9,256*2+1,  1,  0, 27,      0, 10,  0
dw  40,      0,  2,  0,-41,256*2+1,  8,  0
dw  39,256*2+1,  2,  0,-40,256*2+1,  8,  0
dw   1,      0, 12,  0, -2,256*2+1,-49,257
dw   0,256*2+1, 12,  0, -1,256*2+1,-49,257
dw   1,      0,-37,257, -2,256*2+1, 10,  0
dw   0,256*2+1,-37,257, -1,256*2+1, 10,  0
dw   5,      0,-36,257, -8,256*2+1,  8,  0
dw   4,256*2+1,-36,257, -7,256*2+1,  8,  0
dw   1,      0,-24,257,-25,256*2+1,  8,  0
dw -24,256*2+1,-24,257,  4,256*1+1,  8,  0
dw   1,      0,-13,257, -2,256*6+1, 12,  0
dw   0,256*6+1,-13,257, -1,256*6+1, 12,  0
dw   0,256*6+2,-13,257, -1,256*6+1, 12,  0
dw   0,256*6+3,-13,257, -1,256*6+1, 12,  0
dw   0,256*6+4,-13,257, -1,256*6+1, 12,  0
dw   0,256*6+5,-13,257, -1,256*6+1, 12,  0
dw -20,256*2+1,-26,257, 19,256*2+1, 12,  0
dw   2,      0,  2,  0, 08,      0, 08,  0
dw   1,256*2+1,  2,  0, 08,      0, 08,  0

prgobjdsc1  dw pthful1+2,3+0+128
prgobjdsc2  dw pthful2+2,2+4+128
prgobjdsc3  dw prgobjtxt8,2+4+128
prgobjdsc4  dw prgobjtxt9,2+4+128
prgobjdsc5  dw prgobjtxt7,2+4+256+128
prgobjdsc6  dw prgobjtxt7,2+4
prgobjinp1  dw prgobjinpb,0,0,0,0,255,0
prgobjinpb  ds 256

prgobjrada dw prglstfoc,prgobjtxta,2+4+000,prgobjradk
prgobjradb dw prglstfoc,prgobjtxta,2+4+256,prgobjradk
prgobjradk ds 4
prglstfoc  db 0

prgobjdevr  dw 0+0,1000,0,0
prgobjdeva  dw 3,0,prgobjdev1,0,256*0+1,prgobjdevr,0,1
prgobjdevb  dw 3,0,prgobjdev1,0,256*0+1,prgobjdevr,0,1
prgobjdev1  dw 00,00+prgtabdev,01,05+prgtabdev,02,10+prgtabdev,03,15+prgtabdev,04,20+prgtabdev,05,25+prgtabdev,06,30+prgtabdev,07,35+prgtabdev


;### INFO-FENSTER #############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,0,prgicnbig,prgicn16c

;### ERROR-FENSTER ############################################################

prgmsgerr  dw prgmsgerr1,4*1+2
prgmsgerra dw prgmsgerr0,4*1+2,prgmsgerr0,4*1+2

prgtrnend

relocate_table
relocate_end
