dane segment
; CONSTY
	C_spacja 	   	    	  	equ 32d
    C_tab         				equ 09d
    C_dwukropek   			 	equ 58d
	C_max_ilosc_argumentow		equ	2
	 
; Zmienne
	ArgLen	db	(C_max_ilosc_argumentow+1)	dup(?)		; +1 bo nie chce numerowac od 0 tylko od 1
	ArgPos	db	(C_max_ilosc_argumentow+1)	dup(?)
   
    opcja    db ?
    klucz 	db 	17 		dup(?)
	
	kx db ?
	ky db ?
	szachownica db 154 dup(?)	; uzywam tablicy od 1 do 153
	
	znaki db " .o+=*BOX@%&#/^"
; ### komunikaty
    err_liczba_arg db "Zla liczba argumentow! Powinno byc 2 argumenty. $"
	err_nie_hex db "Klucz nie jest w formacie heksadecymalnym $"
	err_dwukropek db "Klucz zle rozdzielony dwukropkami! $"
	err_nie_bin db "Parametr musi byc 0 lub 1 $"
	err_klucz db "Zly format klucza! Poprawny format: xx:xx:xx:xx:xx:xx:xx:xx:xx:xx:xx:xx:xx:xx:xx:xx $"
	
dane ends
      
_stos segment STACK
    db  255  dup(0)
_stos ends
        
	
;##################################
; ************     KOD	  *********
assume cs:kod, ds:dane
kod segment

; ###################
; ****** MAKRA ******
zakoncz macro
    mov ah,4ch
    int 21h
endm

newline macro
    push ax
    push dx
   
    mov ah, 02h
    mov dl, 10d ; new line
    int 21h   
    mov dl, 13d ; carriage return
    int 21h
   
    pop dx
    pop ax
endm
; koniec makr


;################################
; ******** PROCEDURY ***********

; ##### Procedury Pomocnicze
; wypisuje argument z wiersza o numerze zawartym w > di <
wypisz_arg:
	push ax
	push bx
	push cx
	push dx	
		xor ax,ax
		mov al,ArgLen[di]
		mov cx,ax
		mov bl, ArgPos[di]
		wypisz_loop:
			mov dl,es:[bx]
			mov ah,2
			int 21h
			inc bx
		loop wypisz_loop
	pop dx
	pop cx
	pop bx
	pop ax
ret

; inicjalizacja tablic (z powodu problemow z dup(0)) oraz ustawienie gonca na srodku
inicjalizuj:
	push cx
	push si
	
	xor cx,cx
	mov cx,C_max_ilosc_argumentow
	ptla:
		mov si,cx
		mov ArgLen[si],0
		mov ArgPos[si],0
	loop ptla
	
	xor cx,cx
	mov cx,16
	pelta:
		mov si,cx
		mov klucz[si],0
	loop pelta
	
	xor cx,cx
	mov cx,153
	plta:
		mov si,cx
		mov szachownica[si],0 	;zeby tablice wypelnic zerami od 1 do 153
	loop plta
	
	;Ustawienie gonca
		mov kx,9
		mov ky,5
		
	pop si
	pop cx
ret
       
; wypisuje komunikat i konczy program
; pobiera offset argumentu z dx
blad:
    push ds
    push ax
   
    mov ax,seg dane
    mov ds,ax
    mov ah, 09h
    int 21h
   
    pop ax
    pop ds
zakoncz
ret
 
 
; ###### Procedury Parsowania
; Glowna procedura parsowania
parsuj:
	push ax
	push bx
	push cx
	push dx
	
; Ustawienie licznikow	
	mov bx,80h            ;tutaj znajduje sie dlugosc lini argumentow
    
	xor ax,ax
    mov al,es:[bx]        ; wpisujemy  liczbe znakow w linii komend do al
    mov cx, ax            ; a nastepnie ustawiamy na te liczbe licznik cx
	mov di,1			  ; bedziemy przechowywac tu ilosc argumentow
	
; Glowna petla parsowania
	parsowanie_petla:
		inc bx				; w bx mamy adres kolejnych znakow z linii komend        
        mov al,es:[bx]         ; wpisujemy do al znak na ktory wskazuje bx
        mov dl,al       	; i przerzucamy do dl skad bedziemy tym operowac	
			
		call obsluga_znaku

	loop parsowanie_petla
	
	pop dx
	pop cx
	pop bx
	pop ax
ret

;***** Procedury pomocnicze parsowania
; Sprawdza czy znak w dl jest bialy czy tez nie i wywoluje odpowiednie czynnosci
; w di caly czas mamy nr aktualnego argumentu
obsluga_znaku:
	cmp dl,C_spacja		; sprawdzamy czy znak jest bialy
    je bialy_znak
    cmp dl,C_tab
    je bialy_znak
	
		; znak nie jest bialy, wiec zapisujemy go i wracamy
	call dodaj_znak
	jmp gotowe
	
	bialy_znak:
		cmp ds:ArgLen[di],0	
		je gotowe
		inc di			; zaczynamy nowy argument 
gotowe:
ret

; Czynnosci gdy napotykamy niebialy znak
dodaj_znak:
	push ax
	push dx
	cmp ds:ArgLen[di],0		; jezeli to pierwszy znak to zapiszemy tez jego pozycje
	jne zwieksz_arglen		; a jak nie to skok dalej

		call sprawdz_ilosc		; jest to nowy argument, wiec sprawdzamy czy nie mamy ich juz za duzo
	
		mov ax,bx				; dodajemy pozycje rozpoczecia sie argumentu
		mov ds:ArgPos[di],al
	zwieksz_arglen:
		inc ds:ArgLen[di]
	pop dx
	pop ax
ret

; sprawdza liczbe z di czy jest rowna zdefiniowanej stalej
sprawdz_ilosc:
	cmp di,C_max_ilosc_argumentow
	jbe ret_sprawdz_ilosc
	
	push dx
		mov dx, offset err_liczba_arg
		call blad
	pop dx	
	
	ret_sprawdz_ilosc:
ret
; ---- koniec parsowania


; ##### Sprawdzenie poprawnosci wprowadzonych danych
sprawdz_argumenty:
	call sprawdz_klucz
	call sprawdz_zapisz_opcje
ret

; sprawdzenie czy klucz jest podany w poprawnym zapisie xx:xx:xx:...
sprawdz_klucz:
	push ax
	push bx
	push cx
	push dx
	push si
		xor ax,ax
		mov al,ds:ArgPos[2]
		mov bx,ax
		
		xor ax,ax
		mov al,ds:ArgLen[2]
		mov cx,ax
		
		cmp cx,47				; powinno byc 47 znakow
		je klucz_ok
		push dx
			mov dx, offset err_klucz
			call blad
		pop dx
		
		klucz_ok:		
		xor si,si
		petla_klucz:
			mov dl,es:[bx]	; do dl wrzucamy kolejny znak klucza podanego w wierszu polecen
			
			xor ax,ax
			mov ax,cx	
			mov dh,3	; dzielnik
			div dh		; dzieli ax przez wartosc dh
			cmp ah,0	; ah = ax mod dl. sprawdzanie czy co trzeci znak to dwukropek
			
			call czy_dwukropek
			call czy_hex
			
			inc bx
		loop petla_klucz
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret

czy_dwukropek:
	push ax
		cmp ah,0	; to nie miejsce dwukropka
		jne __ret
		
		cmp dl,58
		je __ret
		
		; bledny arg
		push dx
			mov dx, offset err_dwukropek
			call blad
		pop dx		
		
	__ret:
	pop ax
ret
	
czy_hex:
	push ax
		cmp ah,0 
		je _ret		; to miejsce dwukropka
		
		cmp dl,'0'
		jb err_dwukr
		cmp dl,'f'
		ja err_dwukr
		cmp dl,'9'
		jbe _ret
		cmp dl,'a'
		jae _ret
		cmp dl,'F'
		ja err_dwukr
		cmp dl,'A'
		jb err_dwukr
		
		jmp _ret
		err_dwukr:
			push dx
				mov dx, offset err_nie_hex
				call blad
			pop dx
		
	_ret:	
	pop ax
ret

; sprawdzenie poprawnosci podanego paramentru i zapisanie go do zmiennej
sprawdz_zapisz_opcje:
	push ax
	push bx
		xor ax,ax
		mov al,ds:ArgPos[1]
		mov bx,ax
		mov dl,es:[bx]			; wpisujemy znak do dl
		
		xor ax,ax
		mov al,ds:ArgLen[1]		
		cmp al,1				; czy podana opcja to na pewno tylko jedna cyfra
		jne param_err

		cmp dl,'0'					; czy jest binarna?
		jb param_err
		cmp dl,'1'
		ja param_err
		sub dl,48				; konwersja z chara na liczbe
		mov opcja,dl	
		
		jmp _return
		param_err:
		push dx
			mov dx, offset err_nie_bin
			call blad
		pop dx	
		
	_return:
	pop bx
	pop ax
ret


; ##### Konwersja z stringa/hexa do bajtow
; konwersja do hexa i zapis do zmiennej klucz
klucz_HexStrToByte:
	push ax
	push bx
	push cx
	push dx
	push si
		xor ax,ax			; wpisujemy do bx adres pierwszego znaku klucza w linii komend
		mov al,ArgPos[2]
		mov bx,ax
		
		mov cx,16		; dlugosc klucza
		bajtuj:
			xor ax,ax
			mov dl,es:[bx]		; wczytujemy wazniejszy bit liczby
			call HexNbrToByte	; zamieniamy znak na odpowiadajacy bajt
			mov al,dl		; wrzucamy do al zeby pomnozyc to zaraz			
			
			push cx
			mov cl,4		; mnozenie razy 16
			shl al,cl
			pop cx
	;		mov si,16		; mnoznik wazniejszego bitu liczby heksowej
	;		mul si			; jest to byte wiec mnozy si * AL a nie ax
			
			mov dl,es:[bx+1]		; wczytujemy bit parzystosci
			call HexNbrToByte
			add dl,al
			
			mov di,16			; obliczenie indeksu aktualnej czesci klucza
			sub di,cx			
			inc di				; mov klucz[ 16 - cx + 1 ] , al
			mov klucz[di],dl	; zapisujemy skonwertowany wynik 
			
			add bx,3
		loop bajtuj
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret
; przyjmuje znak w >dl< , konwertuje go na liczbe i zwraca przez dl 
HexNbrToByte:
	cmp dl,'9'
	jbe cyfra
	cmp dl,70
	jbe wielka
	
	; mala:
		sub dl,97
		add dl,10
		ret
	wielka:
		sub dl,65
		add dl,10
		ret
	cyfra:
		sub dl,48
		ret
ret


; ###### Operacje na bitach klucza
; Glowna procedura operacji bitowych
rozpracuj_klucz:
	push ax
	push cx
	push dx
	push si
		mov cx,16
		kluczuj:
			mov si,16
			sub si,cx
			inc si				; si = 16-cx+1

			mov dl,ds:klucz[si]
			xor ax,ax
			mov al,dl			; zapamietujemy aktualny znak w al'u, zeby moc go pozniej przywracac w petli bity
			
			push cx
			xor cx,cx
			mov cx,4
			
			bity:		; petla leci po czterech parach bitow w liczbie
				mov dl,al		; dl ulega zmianom w procedurach ponizej, wiec wczytujemy za kazdym razem poprawna liczbe
				
				xor si,si
				mov si,4		; si = 4-cx+1
				sub si,cx
				inc si		
				
				call rozbituj		; czytaj pare bitow numer si
				call rusz_goncem	
			loop bity
			
			pop cx
		loop kluczuj
	pop si
	pop dx
	pop cx
	pop ax
ret

; Rozbija liczbe na odpowiednia pare bitow
; w dl podajemy liczbe, ktora przetwarzamy
; w si podajemy ktora pare bitow liczby ma odczytac
; wynik zwraca w dl. mozliwe wyniki to 0,1,2,3
rozbituj:
	push ax
	push cx	
		xor cx,cx						; ogolna wizja jest taka, ze przesuwamy bity w prawo 
		xor ax,ax						; tak aby te ktore nas interesuja znalazly sie na najmniej znaczacych miejscach
										; i reszte bitow zerujemy. otrzymujemy liczbe z zakresu [0,3]
	; uwzglednienie modyfikacji
		mov al,ds:[opcja]
		mov cl,3					; Latwo bylo wykombinowac wzor dajacy liczbe o jaka przesuwamy w zaleznosci od opcji	->   2* |3*opcja + 1 - si|			
		mul cl					; al=al*cl    czyli   3*opcja	
		inc al	
		call ABS_ax_minus_si		; ax=3*opcja+1 
		mov cl,2
		mul cl						; al = nawias razy 2
		mov cl,al
				
		shr dl,cl			; Przesuwamy bity dl o cl w prawo
		and dl,00000011b	; Zostawiamy tylko bity najmniej znaczace
	pop cx
	pop ax
ret	

; wartosc bezwzgledna z odejmowania ax i si 		|ax-si|
ABS_ax_minus_si:
	push si
		cmp ax,si
		jb zamien
		
	;gdy ax>=si :
		sub ax,si
		pop si
	ret	
	;gdy si>ax :
	zamien:
		sub si,ax
		mov ax,si
		pop si
	ret


; ##### Procedury poruszajace goncem
; 'interfejs' poruszania sie goncem
; pobiera z >dl< gdzie sie ruszyc
rusz_goncem:		
	push bx
	push dx
		call rusz_normalnie			; ruszamy
		call sprawdz_kursor			; sprawdzamy czy nie wyszlismy poza tablice
		call sprawdz_kursor			; sprawdzamy dwa razy, bo jednokrotnie sprawdza tylko jedna z wspolrzednych,
									; a moze byc przypadek ze jestesmy w rogu
		call przelicz_indeks
		inc szachownica[bx]			; zaznaczamy pole jako odwiedzone po raz kolejny
	pop dx
	pop bx
ret

; pobiera z >dl< gdzie sie ruszyc
rusz_normalnie:	
	cmp dl,0	; 00
	je zero
	cmp dl,1	; 01
	je jeden
	cmp dl,2	; 10
	je dwa
	cmp dl,3	; 11
	je trzy
	
	zero:
		dec kx
		dec ky
		ret
	jeden:
		inc kx
		dec ky
		ret
	dwa:
		dec kx
		inc ky
		ret
	trzy:
		inc kx
		inc ky
		ret	
ret

; sprawdza i NAPRAWIA pozycje kursora gdy wychodzi poza tablice 
; (za jednym razem naprawia jedna wspolrzedna (najczestszy przypadek))
sprawdz_kursor:			
	cmp kx,1
	jb kx1
	cmp kx,17
	ja kx17
	cmp ky,1
	jb ky1
	cmp ky,9
	ja ky9
	ret
	kx1:
		mov kx,1
		ret
	kx17:
		mov kx,17
		ret
	ky1:
		mov ky,1
		ret		
	ky9:
		mov ky,9
		ret

; zwraca przez >bx< indeks w tablicy utworzony z pozycji kursora (kx,ky)
przelicz_indeks:
	push ax
	push cx
		xor ax,ax
		mov al,ds:[ky]
		dec ax
		mov cl,17
		mul cl
		add al,ds:[kx]		; ax = 17*(ky-1)+kx
		mov bx,ax
	pop cx
	pop ax
ret	

; ##### Zamiana liczby odwiedzin pol szachownicy na znaki ASCII
ASCII_Art:
	push ax
	push cx
	push dx
	push si
	push di
		mov cx,153
		po_tablicy:
			mov di,cx
			mov dl,ds:[szachownica+di]
			
			cmp dl,14							; byl odwiedzony wiecej niz 14 razy wiec wpisujemy 14
			jbe wpisz
			mov ds:[szachownica+di],14			
			
			wpisz:
				xor ax,ax
				mov al,ds:[szachownica+di]		; wpisujemy liczbe odwiedzin tymczasowo
				mov si,ax						; przepisujemy do rejestru indeksowego
				mov dl,znaki[si]				; zapamietujemy znak odpowiadajacy tej liczbie odwiedzin
				mov ds:[szachownica+di],dl		; wpisujemy do szachownicy znak ASCII
		loop po_tablicy
	pop di
	pop si
	pop dx
	pop cx
	pop ax
ret

; ##### Wypisywanie szachownicy
; Wypisywanie na ekran szachownicy
rysuj_szachownice:
	push ax
	push cx
	push dx
	push di
		call rysuj_linie
		call goniec_start
		call goniec_meta
		
		mov di,1
		mov cx,9
		mov ah,2h
		wiersze:
			mov dl,'|'
			int 21h
		
			push cx
			mov cx,17
			kolumny:
				mov dl,ds:[szachownica+di]
				int 21h
				inc di
			loop kolumny
		
			mov dl,'|'
			int 21h
			newline
		
			pop cx
		loop wiersze
		
		call rysuj_linie
	pop di
	pop dx
	pop cx
	pop ax
ret
rysuj_linie:
	push ax
	push cx
	push dx
		mov ah,2h
		mov dl,'-'
		mov cx,19
		rys:
			int 21h
		loop rys
		newline
	pop dx
	pop cx
	pop ax	
ret

; Wpisywanie literek rozpoczecia i zakonczenia
goniec_start:			
	push bx
		mov bx,77							; indeks srodka szachownicy
		mov szachownica[bx],"S"
	pop bx
ret
goniec_meta:			
	push bx
		call przelicz_indeks				; pobiera aktualna pozycje (konczaca) kursora
		mov szachownica[bx],"E"
	pop bx
ret
; ******** &&&&&&&&&&&&&&&&&&&& KONIEC PROCEDUR &&&&&&&&&&&&&&&&&&&&& ********* ;





;##############################################################################################
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&& --- PROGRAM GLOWNY --- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& ;   
start:   

mov ax,ds				; przepisanie wiersza polecen
mov es,ax				; do segmentu es
mov ax,seg dane			; ustawienie segmentu 
mov ds,ax				; ds jako seg z danymi


call inicjalizuj

call parsuj
call sprawdz_argumenty
call klucz_HexStrToByte

call rozpracuj_klucz

call ASCII_Art
call rysuj_szachownice


koniec:   
    zakoncz
kod ends
end start
