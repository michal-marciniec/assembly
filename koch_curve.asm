;	Michal Marciniec
;	Informatyka AGH 2o11

.387				; aby mozna bylo skorzystac z FCOS

dane segment
; CONSTY
	C_spacja 	   	    	  	equ 32d
    C_tab         				equ 09d
	C_kropka					equ	46d
	C_max_ilosc_argumentow		equ	2d
	C_min_ilosc_argumentow		equ 2d
	C_color						equ 15d
	 
; ZMIENNE
; db - 8bitow
; dw - 16bitow
; dd - 32bitowa dokladnosc (double word)
; dq - 64bitowa dokladnosc
	DZIESIEC				dw 10d
	TRZY					dw 3d
	tmp						dw 0d						; tu przechowujemy cyfry odczytywane z liniii komend i inne tymczasowe wartosci
	smietnik				dd	?			; tu bedziemy wrzucac niepotrzebne rzeczy ze stosu 
	StrToFloatResult 		dd	?

	ArgLen	db	(C_max_ilosc_argumentow+1)	dup(?)		; +1 bo tych tablic nie chce numerowac argumentow od 0 tylko od 1
	ArgPos	db	(C_max_ilosc_argumentow+1)	dup(?)
	
	lenFloat		dd		?			; najpierw odczytujemy parametry jako float
	iternumFloat	dd		?			; aby zaprezentowac konwersje z linii komend i zaokraglanie pozniej na inty
	lenInt			dw		?
	iternumInt		dw		?   
	
	napis db 769 dup(?)
	napisTMP db 769 dup(?)
	napisLen dw ?
   
    x0	dw	0
    y0	dw	0
    xk	dw	0
    yk	dw	0
	alpha dd ?
   
; ### komunikaty
    err_liczba_arg db "Zla liczba argumentow! Powinno byc ich 2 lub 3 ! $"
	err_param db "Niepoprawny parametr wywolania! Dozwolony parametr: '-d' $"
	err_filename_len db "Za dluga nazwa pliku! Max 32 znaki. $"
	err_float_test db "Niepoprawny argument! Porawna forma to liczba nieujemna i niecalkowita$"
	err_len db "Za mala dlugosc! $"
	err_big_args db "Podano za duze parametry! Rysunek nie zmiesci sie na ekranie$"
	
dane ends
      
_stos segment STACK
    db  255  dup(0)
_stos ends
        
	
;##############################
; *********     KOD	  *********
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
	
		FINIT
	
		xor cx,cx
		mov cx,C_max_ilosc_argumentow
		ptla:
			mov si,cx
			dec si					; tablice sa numerowane od 0
			mov ArgLen[si],0
			mov ArgPos[si],0
		loop ptla
		
		mov napisLen,0
		mov cx,768
		ptla2:
			mov di,cx
			mov napis[di],0
			mov napisTMP[di],0
		loop ptla2
		
		mov napis[0],'F'
		mov napis[1],'+'
		mov napis[2],'+'
		mov napis[3],'F'
		mov napis[4],'+'
		mov napis[5],'+'
		mov napis[6],'F'
		mov napisLen,7
		
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
 
 
; ################
; ## PARSOWANIE ##
; ################
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

; ########################################
; ## SPRAWDZENIE I KONWERSJA ARGUMENTOW ##
; ########################################
sprawdz_zapisz_argumenty:
	push ax
	push di
	; iternum
		mov di,1
			call float_test				; zmienia ax
			call StrToFloat				; pobiera z ax
				FLD StrToFloatResult
				FST iternumFloat		; niezdejmujemy ze stosu i zapisujemy do zmiennej
				FRNDINT					; st(0) = (int)st(0)
				FISTP [iternumInt]		; zdejmujemy ze stosu. stos pusty
	
	; len
		mov di,2
			call float_test				; zmienia ax			
			call StrToFloat				; pobiera z ax
				FLD StrToFloatResult
				FST lenFloat
				FRNDINT
				FISTP [lenInt]
			cmp lenInt,0
			je len_err
			cmp lenInt,200
			jae len_err
			
		call sprawdz_wielkosc_argumentow
				
		jmp _ret_sprawdz_argumenty
	; BLEDY:
		len_err:
		push dx
			mov dx, offset err_len
			call blad
		pop dx	
		
	_ret_sprawdz_argumenty:
	pop di
	pop ax
ret

sprawdz_wielkosc_argumentow:
	push ax
	push cx
	push dx
		; sprawdz czy nie za duze paremetry. musi byc spelniony warunek :
		; 2.2 * 3^(iternum-1) * len < 200)				powinno byc 2*sqrt(3) a nie 2.2, ale asm rozciaga rysunki i tutaj 2.2 jest lepszym wspolczynnikiem
		cmp iternumInt,1		; zeby nie bylo problemow z cx=0
		jbe _ret_sprawdz_wielkosc_argumentow
		
		mov tmp,3		; juz wiemy ze iternum jest przynajmniej 2
		FLD1
		FILD tmp			; st = 3, st(1) = 1
		mov dx,iternumInt
		mov cx,dx
		dec cx			; = iternum-1
		pow3:
			FMUL st(1),st
		loop pow3
			; st(1) = 3^(iternum-1)
			
		FSTP smietnik
		FLD1
		mov tmp,5
		FILD tmp
		FDIVP st(1),st				; st = 1/5
		mov tmp,2
		FILD tmp
		FADDP st(1),st				; st = 2.2
		FMULP st(1),st				; st=2.2*3^(iternum-1)		
		
		FLD lenFloat
		FMULP st(1),st				; st = 2.2 * 3^(iternum-1)*len
		
		; czy mniejsze niz 200?
		mov tmp,200
		FICOMP tmp 					; porownuje st z argumentem i zdejmuje st
		FSTSW word ptr [tmp]
		mov ax,[tmp]
		sahf				; przenies AH do rejestru znacznikow PROCESORA
		jnc big_args_err
		
	; BLAD
		jmp _ret_sprawdz_wielkosc_argumentow
		big_args_err:
		push dx
			mov dx, offset err_big_args
			call blad
		pop dx	
		
	_ret_sprawdz_wielkosc_argumentow:	
	pop dx
	pop cx
	pop ax
ret

; ##### Sprawdzenie czy podano liczbe typu float
; czy format [0-9] lub '.'
; czy max. 1 kropka
; WEJSCIE:			di - nr argumentu linii komend do sprawdzenia
; WYJSCIE:			al - ilosc cyfr przed przecinkiem
;					ah - ilosc cyfr po  przecinku
float_test:
	push bx	
	push cx
	push dx
	push di
		mov bl,ds:ArgPos[di]		; zapamietujemy adres do pierwszego znaku
		xor ax,ax			; czyscimy ax bo tam bedziemy formowac wynik
		
		xor cx,cx				
		mov cl,ArgLen[di]		; ustawiamy licznik petli
		
		mov di,0			; w di trzymamy ile wystapilo dotad kropek. zakladam za wymagamy aby wystapila zawsze dokladnie jedna
		
		sprawdz_znak:
			xor dx,dx
			mov dl,es:[bx]
			
			cmp dl,48	; 0
			jb czy_kropka	
			cmp dl,57	; 9
			ja float_test_err
			
			jmp zw_ilosc_cyfr
			czy_kropka:			
				cmp dx,46
				jne float_test_err
				; jezeli to jednak jest kropka to:
				inc di
				cmp di,1
				ja float_test_err
				jmp loop_sprawdz_znak
			
			zw_ilosc_cyfr:
				cmp di,0
				jne po_przecinku
				; jeszcze nie bylo przecinka czyli zwieksz liczbe cyfr przed przecinkiem
				inc al
				jmp loop_sprawdz_znak
				
				po_przecinku:
				inc ah
				
		loop_sprawdz_znak:	
			inc bx
		loop sprawdz_znak
		
		jmp _ret_float_test
		float_test_err:
		push dx
			mov dx, offset err_float_test
			call blad
		pop dx	
		
	_ret_float_test:		
	pop di
	pop dx
	pop cx
	pop bx
ret

; #### Konwersja z linii polecen do float i zapis do zmiennej
; WEJSCIE:			di - nr argumentu
;					al,ah - dane o ilosci cyfr przed/po przecinku
; WYJSCIE:			StrToFloatResult - w tej zmiennej dostajemy wynik
StrToFloat:
	push ax
	push bx
	push cx
	push dx
	push di		
		FLDZ				; st0 = 0.0, inicjalizujemy wynik
		FILD DZIESIEC		; st0 = 10, st1 = 0.0
	
		xor bx,bx
		mov bl,ds:ArgPos[di]
		
		cmp al,0		; gdy pierwszym znakiem jest . to domyslnie z przodu jest 0
		je ulamkowa		
		
		xor cx,cx
		mov cl,al
		dziesiatki:
			push cx
				xor dx,dx
				mov dl,es:[bx]		; wpisujemy cyfre (jako char)
				sub dl,48			; konwertujemy na format liczbowy
				mov tmp,dx			; wpisujemy do zmiennej tymczasowej
				
				FILD tmp			; st0 = cyfra, s1 = 10, st2 = wynik
			
			; wynik = wynik + 10^(cl-1) * tmp	
				;ustaw mnoznik:
				FLD1				; st0 = 1, st1 = cyfra, st2 = 10, st3 = wynik
				cmp cl,1
				je mnoznik10_ustawiony		; unikamy sytuacji w ktorej cx=0
				dec cl
				mnoznik10:
					FMUL st,st(2)		;10^(cl-1)
				loop mnoznik10
				
				mnoznik10_ustawiony:				
				FMUL st,st(1)			; st = 10^(cl-1) * tmp
				FADD st(3),st			; zapisujemy wynik 		
				
				FSTP smietnik 				; zdejmujemy  niepotrzebne wartosci ze stosu
				FSTP smietnik
				
				inc bx
			pop cx
		loop dziesiatki	
		
		ulamkowa:		
		cmp ah,0
		je _ret_StrToFloat
	; jesli jest czesc ulamkowa
		inc bx			; pomijamy kropke
		
		xor cx,cx
		mov cl,ah
		mov di,1		; ktora liczbe po przecinku aktualnie dodajemy
		; bardzo podobne do petli wyzej
		ulamkuj:
			push cx
				xor dx,dx
				mov dl,es:[bx]		; wpisujemy cyfre (jako char)
				sub dl,48			; konwertujemy na format liczbowy
				mov tmp,dx			; wpisujemy do zmiennej tymczasowej

				FILD tmp			; st0 = cyfra, s1 = 10, st2 = wynik
				
			; wynik = wynik + tmp/10^di
				mov cx,di			; tyle razy musimy podzielic cyfre przez 10
				dziel10:
					FDIV st,st(1)	
				loop dziel10
				
				FADD st(2),st
				
				FSTP smietnik
				
				inc bx
				inc di
			pop cx
		loop ulamkuj
		
	_ret_StrToFloat:
		FSTP smietnik				; zdejmujemy 10
		FSTP [StrToFloatResult]		; tutaj mamy ostateczny wynik funkcji
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
ret
; ######################### koniec sprawdzania argumentow


; #########################
; ## GENERUJ NAPIS KOCHA ##
; #########################
generuj_napis:
	push ax
	push cx
	push dx
	push si		; indeks w zmiennej napis
	push di		; indeks w zmiennej napisTMP
	
		cmp iternumInt,0
		je _ret_generuj_napis
		
		mov dx,iternumInt
		mov cx,dx
		iteruj:
			push cx
				xor dx,dx
				mov dx,napisLen
				mov cx,dx
				
				mov si,0
				mov di,0
				
				przepisuj:
					cmp napis[si],'F'
					jne plus_minus
					; zamien 'F':
						mov napisTMP[di],'F'
						inc di
						mov napisTMP[di],'-'
						inc di
						mov napisTMP[di],'F'
						inc di
						mov napisTMP[di],'+'
						inc di
						mov napisTMP[di],'+'
						inc di
						mov napisTMP[di],'F'
						inc di
						mov napisTMP[di],'-'
						inc di
						mov napisTMP[di],'F'
						

					jmp przepisano
					; jezeli + albo - to tylko przepisz znak
					plus_minus:
						mov al,napis[si]
						mov napisTMP[di],al		
					przepisano:
					inc si
					inc di						; tutaj mamy pierwsze wolne miejsce w napisie czyli jednoczesnie dlugosc napisu
				loop przepisuj
			
				call przepiszNapisy
				
			pop cx
		loop iteruj
		
	_ret_generuj_napis:	
	pop di
	pop si
	pop dx
	pop cx
	pop ax
ret

wypisz_napis:
	push ax
	push cx
	push dx
	push di
		mov dx,napisLen
		mov cx,dx
		mov di,0
		mov ah,2
		wypisaj:
			mov dl,napis[di]
			int 21h
			inc di
		loop wypisaj
		newline
		xor	ah, ah				
		int	16h					; oczekujemy na wcisniecie klawisza	
	pop di
	pop dx
	pop cx
	pop ax
ret

; przepisuje napisTMP do napis
; WEJSCIE: 			di 	- dlugosc napisTMP
przepiszNapisy:
	push ax
	push cx
	push di
	
		mov ax,di
		mov napisLen,ax			; zapisujemy nowa dlugosc napisu
		
		mov cx,di
		mov di,0
		
		przepisujNapisy:		
			mov dl,napisTMP[di]
			mov napis[di],dl
			inc di
		loop przepisujNapisy
	
	pop di
	pop cx
	pop ax
ret

; ####################
; ## TRYB GRAFICZNY ##
; ####################
wlacz_tryb_graficzny:
	push ax	
		mov ax,13h
		int 10h
	pop ax
ret
	
wylacz_tryb_graficzny:
	push ax
		xor	ah, ah				
		int	16h					; oczekujemy na wcisniecie klawisza	
		
		mov ax,3
		int 10h
	pop ax
ret

; zapala pixel [si,di]
; WEJSCIE:		si === x
;				di === y
putPixel:
	push ax
	push si
	push di
	push es
	
		mov ax,0a000h
		mov es,ax
		call wyznacz_indeks		; pobiera si,di i zwraca indeks w di
		mov byte ptr es:[di],C_color		; zaladuj do adresu es:[di] jako bajt
		
	pop es
	pop di
	pop si
	pop ax
ret

; WEJSCIE:			si === x
;					di === y
; WYJSCIE:			di = index(si,di)
wyznacz_indeks:
	push ax
	push cx
	push si
		
		xor cx,cx
		mov ax,di
		
		mov cl,8
		shl ax,cl			; *256
		mov cl,6
		shl di,cl			; *64
		add di,ax			; = 320*x
		
		add si,di
		mov di,si			; taka kombinacja bo pomylilem x z y na poczatku i zeby teraz bylo jak najmniej do zmiany

	pop si
	pop cx
	pop ax
ret

; pobiera x0 z ktorych startujemy i kat alfa i wyznacza x do ktorego rysujemy linie
wyznacz_xk_odcinka:
	; xk = len*cos(alpha)+x0
		FILD x0
		FLD lenFloat
		FLD alpha
		
		FCOS				; st=cos(alpha), st(1)=len, st(2)=x0
		FMULP st(1),st		
		FADDP st(1),st
		
		FISTP [xk]
ret

; liczy wartosc funkcji w punkcie podanym w di
; w zaleznosci od x0,y0 i kata alfa
; WEJSCIE:			si - podajemy x
;					x0,y0,alpha
; WYJSCIE:			di - zwracamy y(x)
wyliczY:
	push si
; y(x) = (x-x0)*tg(alpha) + y0
		cmp si,x0
		ja xwiekszy
		
		jmp xmniejszy
		xwiekszy:
			mov tmp, si
			FILD tmp
			FILD x0
			FSUBP st(1),st			; st = x-x0
		jmp wyliczaj
		
		xmniejszy:
			FILD x0
			mov tmp,si
			FILD tmp
			FSUBP st(1),st			; st = x0-x
		
	wyliczaj:
		FLD alpha
		FSIN				
		FLDPI
		FCOS					; tg(alpha) = sin(alpha)/cos(alpha) - bo FPTAN nie dziala, bo tak :<
		FDIVP st(1),st			; st = tg(alpha)		|   st(1) = delta(x)
		
		FMULP st(1),st			; st = (x-x0) * tg(alpha)
		
		FILD y0
		FADDP st(1),st
		FISTP [tmp]
		mov di,tmp
	pop si
ret

; rysuje linie z (x0,y0) do (xk) pod katem alpha
rysuj_linie:
	push ax
	push cx
	push si
	push di
		mov ax,xk
		cmp ax,x0
		jb ujemna_roznica
			mov si,x0		; zaczynamy od x0 i malujemy cx punktow w prawo
			mov cx,xk
			sub cx,x0
			inc cx			; cx = (xk-x0) +1  {aby dlugosc byla 5 to musimy narysowac 6 punktow}
		
		jmp rysuj
		ujemna_roznica:	  ; gdy xk<x0
			mov si,xk		; zaczynamy od xk i malujemy cx punktow w prawo
			mov cx,x0
			sub cx,xk
			inc cx			
			
		rysuj:
			call wyliczY
				; w (si,di) mamy wspolrzedne punktu
			call putPixel
			inc si
		loop rysuj
	pop di
	pop si
	pop cx
	pop ax
ret


linia_cie_kocha:
	push ax
	push cx
	push dx
	push si
	push di
;ustawiamy wartosci poczatkowe
		FLDZ
		FSTP [alpha]

		mov x0,20	
		
		mov ax,lenInt
		mov cx,iternumInt
		cmp cx,2
		jb sety0
			dec cx
			mov dl,3
			mnoz3:
				mul dl
			loop mnoz3
		sety0:
		mov dl,2
		div dl
		mov y0,ax			; gdy iternum>1 to y0=3^(iternum-1)*len / 2    (wlasciwie powinno byc na koncu /(sqrt(3)/2) ~=0.86 
							;ale rysunek nie jest dokladny tylko rozciagniety, wiec 0.5 wystarczy w sam raz)

							
		mov dx,napisLen
		mov cx,dx
		mov di,0
		analizujNapis:
			cmp napis[di],'F'
			je rysowanie
		
			cmp napis[di],'-'
			je minus
			
			cmp napis[di],'+'
			je plus
			
			rysowanie:	
				push si
				push di
					mov si,x0
					mov di,y0
					call wyznacz_xk_odcinka
					call rysuj_linie
					
				; zapisujemy wspolrzedne konca jako nowe wspolrzedne poczatku
					call wyznacz_xk_odcinka
					mov si,xk		; wyliczY pobiera x z si
					call wyliczY
					mov y0,di
					mov x0,si
				pop di 
				pop si
			jmp next
			
			minus:	
				FLD alpha				; st = alpha
				
				FLDPI
				FILD TRZY
				FDIVP st(1),st			; st = pi/3    | st(1) = alpha
				
				FADDP st(1),st			; st = alpha + pi/3
				FSTP [alpha]
			jmp next
			
			plus:	
				FLD alpha
				
				FLDPI
				FILD TRZY
				FDIVP st(1),st
				
				FSUBP st(1),st				
				FSTP [alpha]
			
		next:			
			inc di
		loop analizujNapis
		
	pop di
	pop si
	pop dx
	pop cx
	pop ax
ret



;##############################################################################################
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&& --- PROGRAM GLOWNY --- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& ;   
start:   

mov ax,ds				; przepisanie wiersza polecen
mov es,ax				; do segmentu es
mov ax,seg dane			; ustawienie segmentu 
mov ds,ax				; ds jako seg z danymi

call inicjalizuj

call parsuj

call sprawdz_zapisz_argumenty

call generuj_napis
call wypisz_napis

call wlacz_tryb_graficzny

call linia_cie_kocha

koniec:   
	call wylacz_tryb_graficzny
    zakoncz
kod ends
end start
