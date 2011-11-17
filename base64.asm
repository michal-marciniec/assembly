;	Michal Marciniec
;	Informatyka AGH 2o11

dane segment
; CONSTY
	C_spacja 	   	    	  	equ 32d
    C_tab         				equ 09d
	C_max_ilosc_argumentow		equ	3
	C_min_ilosc_argumentow		equ 2
	C_buff_size					equ 30760		; 30760 = 30*1024 = 30KB
	
	BASE64_CHARS db 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
	 
; ZMIENNE
	ArgLen	db	(C_max_ilosc_argumentow+1)	dup(?)		; +1 bo tych tablic nie chce numerowac argumentow od 0 tylko od 1
	ArgPos	db	(C_max_ilosc_argumentow+1)	dup(?)
	IsParam db 	?		; 1 jesli podano parametr, 0 jezeli bezparametrowo
						; czyli tez jezeli IsParam=0 to podano 3 argumenty a jesli =1 to znaczy ze lacznie 4
	
	;Nazwa musi byc zakonczona bajtem zerowym!
	InputFile db 32 dup(?),0
	OutputFile db 32 dup(?),0
	
	InputPointer dw ?
	OutputPointer dw ?		

	readBuffer db C_buff_size dup(?)		
	last_read_size dw ?				; pomocniczo, ile jest w buforze po ostatnim wczytaniu
	readBufferPointer dw ?			; trzyma indeks pierwszego NIECZYTANEGO jeszcze znaku z readBuffera (czyli trzyma info ile juz przeczytalismy)
	
	writeBuffer db C_buff_size dup(?)
	writeBufferPointer dw ?
	
	tab3 db 3 dup(?)			; tu bedziemy przechowywac wczytywane 3 znaki do KODOWANIA albo 3 znaki przeznaczone do zapisu ZDEkodowanego tekstu
	tab4 db 4 dup(?)			; tu bedziemy przechowywac wczytywane 4 znaki do DEKODOWANIA albo 4 znaki przeznaczone do zapisu ZAkodowanego
	
   
; ### komunikaty
    err_liczba_arg db "Zla liczba argumentow! Powinno byc ich 2 lub 3 ! $"
	err_param db "Niepoprawny parametr wywolania! Dozwolony parametr: '-d' $"
	err_filename_len db "Za dluga nazwa pliku! Max 32 znaki. $"
	err_file_not_exist db "Plik z danymi nie istnieje! $"
	err_file_not_closed db "Zamykanie pliku nie powiodlo sie! $"
	err_file_not_created db "Utworzenie pliku nie powiodlo sie! $"
	err_len_calc db "Blad podczas okreslania dlugosci pliku! $"
	err_rw_arg db "Podano zly parametr wywolania funkcji wczytaj/zapisz! $"
	err_readfile db "Blad podczas odczytu z pliku wejsciowego! $"
	err_writefile db "Blad podczas zapisu do pliku wynikowego! $"
	err_cursor db "Blad podczas operacji zwiazanych z obsluga kursora pliku $"
	err_decodingfile_format db "Tekst do zdekodowania nie jest zapisany w BASE64! $"
	
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

; inicjalizacja tablic (z powodu problemow z dup(0))
inicjalizuj:
	push cx
	push si
	
	xor cx,cx
	mov cx,C_max_ilosc_argumentow
	ptla:
		mov si,cx
		dec si					; tablice sa numerowane od 0
		mov ArgLen[si],0
		mov ArgPos[si],0
	loop ptla
	
	xor cx,cx
	mov cx,32
	ptl:
		mov si,cx
		dec si
		mov InputFile[si],0
		mov OutputFile[si],0
	loop ptl
	
	mov ax, C_buff_size
	mov readBufferPointer,ax		; wskaznik bufora ustawiony na koncu informuje ze nie ma (juz) znakow w buforze
	mov writeBufferPointer,0
	mov last_read_size,0

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
		
		call usun_biale_na_koncu
		call sprawdz_ilosc	
		
		sub di,2						
		xor dx,dx					; jezeli podano 3 argumenty to ustawiamy, ze podano (teoretycznie) parametr (IsParam=1)
		mov dx,di					; jezeli 2 argumenty to ustawiamy 0
		mov IsParam,dl				
	
	pop dx
	pop cx
	pop bx
	pop ax
ret

;***** Procedury pomocnicze parsowania
; jezeli sa biale znaki na koncu to zmniejsza liczbe parametrow
usun_biale_na_koncu:
	push ax
		call czy_bialy
		cmp ax,0
		je nie_ma
		dec di
	nie_ma:	
	pop ax
ret

; pobiera z dl znak
; zwraca w ax 1 gdy bialy
czy_bialy:
	xor ax,ax
	mov ax,0
	cmp dl,C_spacja		; sprawdzamy czy znak jest bialy
    je bialy
    cmp dl,C_tab
    je bialy
	ret
	bialy:
		mov ax,1
ret

; Sprawdza czy znak w dl jest bialy czy tez nie i wywoluje odpowiednie czynnosci
; w di caly czas mamy nr aktualnego argumentu
obsluga_znaku:
	push ax		; czy_bialy zmienia ax
		call czy_bialy
		cmp ax,1
		je bialy_znak
			
	; znak nie jest bialy, wiec zapisujemy go i wracamy
		call dodaj_znak
		jmp gotowe
		
		bialy_znak:
			cmp ds:ArgLen[di],0	
			je gotowe
			inc di			; zaczynamy nowy argument 
	gotowe:
	pop ax
ret

; Czynnosci gdy napotykamy niebialy znak
dodaj_znak:
	push ax
	push dx
	cmp ds:ArgLen[di],0		; jezeli to pierwszy znak to zapiszemy tez jego pozycje
	jne zwieksz_arglen		; a jak nie to skok dalej	
		mov ax,bx				; dodajemy pozycje rozpoczecia sie argumentu
		mov ds:ArgPos[di],al
	zwieksz_arglen:
		inc ds:ArgLen[di]
	pop dx
	pop ax
ret

; sprawdza liczbe z di czy miesci sie w zadanym przedziale
sprawdz_ilosc:
	cmp di,C_max_ilosc_argumentow
	ja zla_ilosc
	cmp di,C_min_ilosc_argumentow
	jb zla_ilosc	
	ret
	
	zla_ilosc:
	push dx
		mov dx, offset err_liczba_arg
		call blad
	pop dx	
ret
; ------------------- koniec parsowania -----------------------

; ##### Sprawdzenie poprawnosci wprowadzonych danych
sprawdz_argumenty:
	call sprawdz_param
	call zapisz_nazwy_plikow
ret
; sprawdza czy podano parametr i czy jest on poprawny
sprawdz_param:
	push ax
	push bx	
		xor ax,ax
		mov ah,IsParam
		cmp ah,0		; nie podano parametru
		je _ret_param	
		; podano 4 argumenty, wiec sprawdzamy poprawnosc parametru
				cmp ArgLen[1],2
				jne param_err
				
				mov bl,ds:ArgPos[1]
				mov ax,es:[bx]							
				cmp ax,642Dh			; 64 == '-', 2D == 'd'
				jne param_err

				jmp _ret_param
				param_err:
				push dx
					mov dx, offset err_param
					call blad
				pop dx		
	_ret_param:
	pop bx
	pop ax
ret

zapisz_nazwy_plikow:
	push si
	push di
	push bx
	push cx
	push dx
	; INPUT-NAME
		xor ax,ax
		mov al,ds:IsParam
		mov si,ax
		inc si				; ustawiamy si na pierwsza nazwe pliku (jesli IsParam=1 to wtedy jest ona drugim arugmentem w linii komend)				
				
		xor bx,bx
		mov bl,ds:ArgPos[si]
		mov dl,es:[bx]

		xor ax,ax
		mov al,ds:ArgLen[si]
		mov cx,ax		
		cmp cx,32			; dozwalamy tylko nazwy o dlugosci 32
		ja filename_err
		zapis_inputname:
			mov di,ax					; podajemy dlugosc argumentu
			sub di,cx					; obliczamy indeks
			mov dl,es:[bx]			; przepisujemy znak do dl			
			mov ds:InputFile[di],dl		; i wpisujemy go do zmiennej
			inc bx						; idziemy do kolejnego elementu
		loop zapis_inputname
		
	; OUTPUT-NAME
		inc si
		xor bx,bx
		mov bl,ArgPos[si]
		
		xor ax,ax
		mov al,ArgLen[si]
		mov cx,ax		
		
		cmp cx,32
		ja filename_err
		zapis_outputname:
			mov di,ax					; podajemy dlugosc argumentu
			sub di,cx					; obliczamy indeks
			mov dl,es:[bx]				; przepisujemy znak do dl
			mov ds:OutputFile[di],dl		; i wpisujemy go do zmiennej
			inc bx						; idziemy do kolejnego elementu
		loop zapis_outputname
		
		jmp _ret_zapisz_nazwy_plikow
		filename_err:
		push dx
			mov dx, offset err_filename_len
			call blad
		pop dx		

	_ret_zapisz_nazwy_plikow:
	pop dx
	pop cx
	pop bx
	pop di
	pop si
ret

; ##################### OPERACJE NA PLIKACH

; ## Otwieranie pliku
; wejscie: 	dx - offset nazwy pliku do otwarcia
otworz_plik:
	push ax
		mov	ah, 3dh			; funkcja otwierania pliku
		mov	al, 00000000b	; dostep tylko do odczytu
		int	21h				; proba otwarcia
		jc	err_otworz_plik		; sprawdzamy, czy nie ma bledu. flaga jest w cf

		mov	ds:[InputPointer], ax
		
		jmp _ret_otworz_plik
		err_otworz_plik:
		push dx
			mov dx, offset err_file_not_exist
			call blad
		pop dx	
		
	_ret_otworz_plik:
	pop ax
ret

; ## Zamykanie pliku
; wejscie: bx - uchwyt do pliku
zamknij_plik:
	push ax
		mov	ah, 3eh		; funkcja zamkniecia pliku
		int	21h			; proba zamkniecia
		jnc	_ret_zamknij_plik		; nie ma bledow
	; nie udalo sie:
		push dx
			mov dx, offset err_file_not_closed
			call blad
		pop dx	
		
	_ret_zamknij_plik:
	pop ax
ret

; ## Utworzenie pliku
; wejscie: 	dx - offset do nazwy pliku
utworz_plik:
	push ax
	push cx
		mov	ah, 3ch		; funckja utworzenia pliku
		xor	cx, cx		; w cx podajemy typ pliku.  0 - plik 'normalny'
		int	21h			; utworzenie pliku
		jc	utworz_plik_err		
		mov	ds:[OutputPointer], ax
		
		jmp _ret_utworz_plik
		utworz_plik_err:
		push dx
			mov dx, offset err_file_not_created
			call blad
		pop dx		
	_ret_utworz_plik:
	pop cx
	pop ax
ret

; Okresla aktualna pozycje na jakiej jest kursor (wzgledem poczatku)
; wejscie:		bx - uchwyt do pliku
; wyjscie		ax - przesuniecie
wyznacz_pozycje:
	push cx
	push dx	
		mov ah,42h		; funkcja zmiany wskaznika pliku
		mov al,1		; przesuniecie wzgledem AKTUALNEJ POZYCJI kursora
		xor cx,cx 		; adresowanie przesuniecia jest 32-biotowe CX:AX a my chcemy przesunac tylko o '-1'
		mov dx,0
		int 21h			; teraz w ax mamy przesuniecie wzgledem poczatku
		jnc _ret_wyznacz_pozycje
		
		push dx
			mov dx, offset err_cursor
			call blad
		pop dx	
		
	_ret_wyznacz_pozycje:	
	pop dx
	pop cx
ret

; Ustawia kursor na pozycji przesunietej o wartosc w dx wzgledem POCZATKU pliku
; wejscie:		bx - uchwyt do pliku
;				dx - przesuniecie
ustaw_kursor:
	push ax
	push cx
	push dx	
		mov ah,42h		; funkcja zmiany wskaznika pliku
		mov al,0		; przesuniecie wzgledem POCZATKU pliku
		xor cx,cx 		; adresowanie przesuniecia jest 32-biotowe CX:AX a my chcemy przesunac tylko o '-1'
				; przesuniecie mamy w dx
		int 21h			; przesuwamy
		jnc _ret_ustaw_kursor
		
		push dx
			mov dx, offset err_cursor
			call blad
		pop dx	
		
	_ret_ustaw_kursor:	
	pop dx
	pop cx
	pop ax
ret


; ## Wyznacza dlugosc pliku
; wejscie:		bx - uchwyt do pliku
; wyjscie:		ax - dlugosc pliku
wyznacz_dlugosc:
	push cx
	push dx
	; zapamietujemy aktualna pozycje kursora
		call wyznacz_pozycje				
		mov dx,ax
		push dx					
	; wyznaczamy dlugosc pliku
		mov ah,42h		; funkcja zmiany wskaznika pliku
		mov al,2		; przesuniecie wzgledem KONCA pliku
		xor cx,cx 		;  adresowanie przesuniecia jest 32-biotowe CX:AX a my chcemy przesunac tylko o '-1'
		mov dx,0
		int 21h			; teraz w dx:ax mamy dlugosc pliku (przesuniecie wzgledem poczatku)
		jc wyznacz_dlugosc_err		; sprawdzamy czy nie bylo bledow
	; Musimy koniecznie wrocic z powrotem na pozycje na jakiej bylismy przed wyznaczaniem dlugosci !!!
		pop dx
		call ustaw_kursor
		
		jmp _ret_wyznacz_dlugosc
		wyznacz_dlugosc_err:
		push dx
			mov dx, offset err_len_calc
			call blad
		pop dx	
		
	_ret_wyznacz_dlugosc:
	pop dx
	pop cx
ret

; ## Wczytuje z pliku do podanej tablicy zadana liczbe bajtow
; wejscie:		bx - uchwyt pliku
;				dx - offset tablicy do ktorej zapisujemy
;				cx - liczba bajtow do wczytania
czytaj:
	push ax
		mov ah,3fh		;  funkcja zajmujaca sie wczytywaniem plikow
		int 21h	
		jc czytaj_err
	pop ax
	ret
		czytaj_err:
		push dx
			mov dx, offset err_readfile
			call blad
		pop dx
	pop ax
	ret

; ## Zapisuje z podanej tablicy zadana liczbe bajtow do pliku
; wejscie:		bx - uchwyt pliku
;				dx - offset tablicy z ktorej czytamy
;				cx - liczba bajtow do zapisania
zapisz:
	mov ah,40h		;  funkcja zajmujaca sie zapisem do pliku
	int 21h
	jc zapisz_err
  ret
	zapisz_err:
	push dx
		mov dx, offset err_writefile
		call blad
	pop dx
  ret
  
  
  
; ###### Obsluga bufora
czytaj_do_bufora:
	push dx
	push cx
	push bx
	push ax
		mov dx,offset readBuffer
		mov cx,C_buff_size
		mov bx,ds:InputPointer
		mov ah,3Fh
		int 21h
		mov readbufferPointer,0
		mov last_read_size,ax	

		jnc _ret_czytaj_do_bufora
		push dx
			mov dx, offset err_readfile
			call blad
		pop dx
		
	_ret_czytaj_do_bufora:
	pop ax
	pop bx
	pop cx
	pop dx
ret

; Wczytuje  z bufora do podanej tablicy
; wejscie:			cx - ile znakow
;					dx - offset tablicy do ktorej czytamy
czytaj_z_bufora:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
		mov bx,dx		; gdzie zapisujemy
		mov di,cx		; ile jeszcze mamy wczytac
		
		czytamy:
			mov cx,C_buff_size
			sub cx,readBufferPointer			; ile mamy w tym buforze jeszcze danych pozostalych do odczytania
			
			cmp cx,0			; zapobiega sytuacji ze podamy cx=0 do petli
			je bufor_pusty
			
			cmp di,cx				
			jbe szybki_odczyt			; wystarczy nam danych w tym buforze
									; w przeciwnym wypadku czytamy tyle ile pozostalo i wczytujemy kolejna porcje danych do bufora
			mov si,readbufferPointer
			sub di,cx					; odejmujemy od di cx , bo tyle zaraz wczytamy
			; wczytujemy cx znakow
			zapisz_czesc:
				mov al,readBuffer[si]
				mov [bx],al
				inc bx
				inc si
			loop zapisz_czesc
			
			bufor_pusty:
				call czytaj_do_bufora
		jmp czytamy
			
		
		szybki_odczyt:
			mov cx,di
			mov si,readbufferPointer
			add readbufferPointer,di
			odczytuj:
				mov al,readBuffer[si]
				mov [bx],al
				inc bx
				inc si
			loop odczytuj		
			
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret 
  
; ## zapisuje bufor do pliku
;wejscie:	cx - ile znakow zapisac
zapisz_z_bufora:
	push dx
	push cx
	push bx
	push ax
		mov dx,offset writeBuffer
		mov bx,OutputPointer
		mov writeBufferPointer,0
		call zapisz
	pop ax
	pop bx
	pop cx
	pop dx
ret

; Zapisuje z tablicy do bufora
; wejscie:			cx - ile znakow
;					dx - offset tablicy z ktorej czytamy
zapisz_do_bufora:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
		mov bx,dx
		mov di,cx					; ile jeszcze mamy zapisac
	; sprawdzamy czy wystarczy nam miejsca w buforze i w zaleznosci od tego odpowiednio postepujemy
		zapisujemy:
			mov cx,C_buff_size
			sub cx,writeBufferPointer
			
			cmp cx,0				; gdy nie mamy juz miejsca w buforze to zapisujemy jego zawartosc na dysk
			je czysc_bufor
			
			cmp cx,di					; if(size-pointer)<= di		wtedy dopisujemy troche i zerujemy bufor
			jae krotki_zapis			; jesli nie to starczy nam miejsca w buforze na dopisanie zawartosci tablicy
			
			;duzy_zapis:
			; teraz w cx mamy ile znakow mozemy dopisac do aktualnego bufora
			xor ax,ax
			mov si,ds:writeBufferPointer
			sub di,cx		; zmniejszamy liczbe znakow ktore mamy dopisac 
			zapisztroche:
				mov al,[bx]
				mov writeBuffer[si],al
				inc si
				inc bx
			loop zapisztroche
			
			czysc_bufor:
			push cx
				mov cx,C_buff_size
				call zapisz_z_bufora			; tu zostanie wyzerowany pointer
			pop cx
		jmp zapisujemy
				
		krotki_zapis:
			xor ax,ax
			mov cx,di
			mov si,writeBufferPointer
			add writeBufferPointer,cx
			zapisujbuf:
				mov al,[bx]
				mov writeBuffer[si],al
				inc si
				inc bx
			loop zapisujbuf
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret
  
  
  
; Procedury opakowujace zwiazane konkretnie z rozwiazywanym zadaniem wywolujace procedury ogolne do zapisu i odczytu
; wejscie:		w >si< przyjmuje opcje wywolania i w zaleznosci od niej wywolywana jest ogolna procedura z odpowiednimi dla zadania parametrami
; pierwsza cyfra ozacza wielkosc tablicy, druga rodzaj funkcji (1-wczytaj, 2-zapisz)
wczytaj_zapisz:
	push bx
	push cx
	push dx
		cmp si,31
		jne check32
			mov dx,offset tab3
			mov cx,3
			call czytaj_z_bufora			
			jmp _ret_wczytaj_zapisz
		check32:
		cmp si,32
		jne check41
			mov dx,offset tab3
			mov cx,3
			call zapisz_do_bufora
			jmp _ret_wczytaj_zapisz
		check41:
		cmp si,41
		jne check42
			mov dx,offset tab4
			mov cx,4
			call czytaj_z_bufora
			jmp _ret_wczytaj_zapisz
		check42:
		cmp si,42
		jne rw_arg_err
			mov dx,offset tab4
			mov cx,4
			call zapisz_do_bufora
			jmp _ret_wczytaj_zapisz

		rw_arg_err:
		push dx
			mov dx, offset err_rw_arg
			call blad
		pop dx	
		
_ret_wczytaj_zapisz:
	pop dx
	pop cx
	pop bx
ret



; ##################### BASE64
; Procedura kodujaca
koduj:
	push ax
	push cx
	push dx
	; x' = x shr 2 
		xor ax,ax
		mov al,ds:tab3[0]
		mov cl,2
		shr al,cl
		mov ds:tab4[0],al
		
	; y' = ( (x and 00000011b) shl 4 ) or ( y and 11110000b) shr 4)
		xor ax,ax
		xor dx,dx
		mov al,ds:tab3[0]
		and al,00000011b
		mov cl,4
		shl al,cl
		mov dl,ds:tab3[1]
		and dl,11110000b
		shr dl,cl
		or al,dl
		mov ds:tab4[1],al
		
	; z' = ( (y and 00001111b) shl 2 ) or ( (z and 11000000b) shr 6)
		xor ax,ax
		xor dx,dx
		mov al,ds:tab3[1]
		and al,00001111b
		mov cl,2
		shl al,cl
		mov dl,ds:tab3[2]
		and dl,11000000b
		mov cl,6
		shr dl,cl
		or al,dl
		mov ds:tab4[2],al	
		
	; w' = z and 00111111b
		xor ax,ax
		mov al,ds:tab3[2]
		and al,00111111b
		mov ds:tab4[3],al		
	pop dx
	pop cx
	pop ax
ret

; Procedura dekodujaca
dekoduj:
	push ax
	push cx
	push dx
	;    x' = (x shl 2) or (y shr 4)
		xor ax,ax
		xor dx,dx
		mov al,ds:tab4[0]
		mov cl,2
		shl al,cl
		
		mov dl,ds:tab4[1]
		mov cl,4
		shr dl,cl
		
		or al,dl
		mov ds:tab3[0],al
		
	;    y' = (y shl 4) or ((z shr 2) and 15)
		xor ax,ax
		xor dx,dx
		mov al,ds:tab4[1]
		mov cl,4
		shl al,cl
		
		mov dl,ds:tab4[2]
		mov cl,2
		shr dl,cl
		and dl,00001111b
		
		or al,dl
		mov ds:tab3[1],al
		
	;    z' = ((z shl 6) and 192) or (w and 63)
		xor ax,ax
		xor dx,dx
		mov al,ds:tab4[2]
		mov cl,6
		shl al,cl
		and al,11000000b
		
		mov dl,ds:tab4[3]
		and dl,00111111b
		
		or al,dl
		mov ds:tab3[2],al
		
	pop dx
	pop cx
	pop ax
ret

; Zamienia wartosci liczbowe otrzymane po konwersji w tab4 na znaki BASE64 o tym indeksie 
zamien_na_base:
	push ax
	push cx
	push di
	push si
		xor cx,cx
		mov di,4
		mov cx,4
		zamienNa:
			xor ax,ax
			dec di					; bo nie mozemy sie odwolac tab4[cx-1]
			mov al,ds:tab4[di]
			mov si,ax				; w si mamy zachowany indeks znaku
			xor ax,ax
			
			mov al,BASE64_CHARS[si]
			mov ds:tab4[di],al		; aktualizujemy zawartosc
		loop zamienNa
		
_ret_zamien_NA_base:
	pop si
	pop di
	pop cx
	pop ax
ret

zamien_z_base:
	push ax
	push cx
	push di
	push si
		mov di,0			; indeks w tab4
		xor cx,cx
		mov cx,4			; przeksztalcamy znaki z tablicy tab4 na ich indeksy
		zamienZ:
			mov si,-1
			zamieniaj:
				inc si			
				cmp si,64
				jae czy_rownasie			; jesli przegladnelismy cala tablicy BASE64 i nie ma w niej tego znaku
											; to znaczy ze musi to byc '=' albo jest to niepoprawny znak
				mov al,ds:tab4[di]
				cmp al,BASE64_CHARS[si]
			jne zamieniaj
			
			jmp zwykly_znak
			czy_rownasie:				
				cmp ds:tab4[di],'='
				jne zamienZ_err
				mov si,0					; tu podajemy na co zamieniamy znak '='
			
			zwykly_znak:				; aktualizujemy zawartosc z base na znak 'normalny'
			mov ax,si
			mov ds:tab4[di],al
			inc di
		loop zamienZ
		
	jmp _ret_zamien_z_base
	zamienZ_err:	
		xor cx,cx
		push dx
			mov dx, offset err_decodingfile_format
			call blad
		pop dx	
	
_ret_zamien_z_base:	
	pop si
	pop di
	pop cx
	pop ax
ret


kodowanie_glowne:
	push si
		mov si,31				; wczytaj 3 znaki
		call wczytaj_zapisz
			
		call koduj				; zakoduj
		call zamien_na_base		; zamien na odpowiedniki w BASE64
		
		mov si,42				; zapisz
		call wczytaj_zapisz
	pop si
ret
kodowanie:
	push ax
	push cx
		call czytaj_do_bufora 
		
		kodowanie_pelne:						; dopoki wczytany jest pelny bufor to sobie koduj
			mov ax,C_buff_size				
			cmp last_read_size,ax
			jne koncowka
			call kodowanie_glowne
		jmp kodowanie_pelne
		
		koncowka:								; gdy juz nam sie konczy plik (czyli bufor jest tylko po czesci zapelniony)
			mov ax,last_read_size	
			sub ax,readbufferPointer			
			
			xor cx,cx							; wyznacz ile razy musimy wykonac jeszcze kodowan trojek
			wyznaczaj_cx:
				cmp ax,3
				jb juz
				inc cx
				sub ax,3
			jmp wyznaczaj_cx
				
			juz:
			mov ah,al					; chce miec w ah ilosc pozostalych znakow

			cmp cl,0					; nie mozna miec - w cx bo bedzie petla nieskonczona
			je dopisz_rownasie
			
			kod_koncowka:
				call kodowanie_glowne
			loop kod_koncowka

											; uzupelnij	znaki ktore nie maja 'trojki'
		dopisz_rownasie:
		cmp ah,0			; ah = ax mod cl okresla ile trzeba dopisac znakow do ostatniej porcji danych
		je _ret_kodowanie
		
	; -----Kodowanie niepelnej czesci na koncu pliku	
		; zeruj tab3
		mov cx,3
		mov si,2
		zeruj_tab3:
			mov ds:tab3[si],0
			dec si
		loop zeruj_tab3
		
		; wczytaj >ah< znakow z pliku do tab3
		mov dx,offset tab3
		xor cx,cx
		mov cl,ah
		call czytaj_z_bufora
		
		call koduj
		call zamien_na_base
		
		; dopisz '=' na miejscach dopelnianych
		mov ds:tab4[3],'='
		cmp ah,2
		je dopisz
			mov ds:tab4[2],'='
		dopisz:
		mov si,42
		call wczytaj_zapisz
	; ----------------------------	
		
	_ret_kodowanie:
	pop ax
	pop cx
ret

dekodowanie_glowne:
	push si
		mov si,41
		call wczytaj_zapisz
			
		call zamien_z_base		
		call dekoduj
	
		mov si,32
		call wczytaj_zapisz
	pop si
ret
dekodowanie:
	push ax
	push cx
	push si
		call czytaj_do_bufora 
		
		dekodowanie_pelne:						; dopoki wczytany jest pelny bufor to sobie dekoduj
			mov ax,C_buff_size				
			cmp last_read_size,ax
			jne dekoncowka
			call dekodowanie_glowne
		jmp dekodowanie_pelne
		
		dekoncowka:								; gdy juz nam sie konczy plik (czyli bufor jest tylko po czesci zapelniony)
			mov ax,last_read_size	
			sub ax,readbufferPointer			
			
			xor cx,cx							; wyznacz ile razy musimy wykonac jeszcze dekodowan czworek
			dewyznaczaj_cx:
				cmp ax,4
				jb dejuz
				inc cx
				sub ax,4
			jmp dewyznaczaj_cx

			dejuz:
			cmp cl,0					; nie mozna miec - w cx bo bedzie petla nieskonczona
			je _ret_dekodowanie
			
			dekod_koncowka:
				call dekodowanie_glowne
			loop dekod_koncowka		
		
	_ret_dekodowanie:
	pop si
	pop ax
	pop cx
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

mov dx,offset InputFile
call otworz_plik
mov dx,offset OutputFile
call utworz_plik

mov bx,ds:InputPointer
cmp IsParam,1
je dekoding
	call kodowanie
	jmp zamykamy
dekoding:
	call dekodowanie

zamykamy:	
mov cx,writeBufferPointer				; wyrzuca to co zostalo w buforze
call zapisz_z_bufora
call zamknij_plik

koniec:   
    zakoncz
kod ends
end start
