    ;LIST P=PIC18F4321	F=INHX32
    #include <p18f4321.inc>
    
    
    ;	Fosc = 40Mhz	    
    ;	Tinst = 4/40Mhz = 100nS
    ;	Int de Timer0 cada 100uS
    CONFIG  OSC=HSPLL		; Osc -> HSPLL=40Mhz
    CONFIG  PBADEN=DIG
    CONFIG  WDT=OFF
    ;CONFIG  MCLR=ON
    
    ORG 0x0000
    GOTO    MAIN
    ORG 0x0008
    GOTO    HIGH_RSI
    ORG 0x0018
    RETFIE  FAST
    
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; -----------------------  VAR FLASH   -----------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Temps a restar per nota equivalent al total de notes (uS)
    ORG	0x003A
    DB .200, .100
    DB .67, .50
    DB .40, .33
    DB .29, .25
    DB .22, .20
    DB .18, .17
    DB .15, .14
    DB .13, .12
    
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; -----------------------  VAR RAM   -------------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
CURRENT_NOTE		    EQU 0x01	; Note actual
NOTE_LISTENED		    EQU 0x02	; Nota que s'escolta per l'spk
LENGTH			    EQU 0x03	; Duration actual
RAM_VALUE_AUX1		    EQU 0x04	; Variable aux per guardar el que es llegeix de la RAM
RAM_VALUE_AUX2		    EQU 0x05	; Variable aux per guardar el que es llegeix de la RAM
N_NOTES			    EQU 0x06	; Numero de Notes introduides
N_NOTES_REPRODUIDES	    EQU 0x07	; Numero de notes reproduides
COUNT_10uS		    EQU 0x08	; Counter auxiliar de 10uS
COUNT_500mS_LOW		    EQU 0x09	; Counter per als 500mS LOW
COUNT_500mS_HIGH	    EQU 0x0A	; Counter per als 500mS	HIGH
FLAG_500mS		    EQU 0x0B	; Flag per veure si han passat 500mS
COUNT_3S_LOW		    EQU 0x0C	; Counter per als 500mS LOW
COUNT_3S_HIGH		    EQU 0x0D	; Counter per als 500mS	HIGH
FLAG_3S			    EQU 0x0E
COUNT_1mS		    EQU 0x0F
COUNT_LENGTH_LOW	    EQU 0x1A
COUNT_LENGTH_HIGH	    EQU 0x1B
FLAG_COUNT_LENGTH_STARTED   EQU 0x1C		
FLAG_COUNT_LENGTH_FINISHED  EQU 0x1D
ERROR_MADE		    EQU 0x1E
DISTANCE		    EQU 0x1F	; Distancia en cm del ultrasons
COUNT_17mS		    EQU 0x20
COUNT_SERVO		    EQU 0x21
TIME_TO_SUBTRACT_SERVO	    EQU 0x22
COUNT_500uS		    EQU 0x23
ENABLE_COUNT_17mS	    EQU 0x24
SERVO_ENDED_COUNT	    EQU 0x25
COUNT_SERVO_2		    EQU 0x26		    
NOTE_PERIOD		    EQU 0x27	; Var que estableix el periode del spk
COUNT_N_RSI_ECHO	    EQU 0x28	; Numero de cops que ha arribat la RSI
TRIGGER_SENT		    EQU 0x29	; Flag conforme trigger ha estat enviat
ECHO_RECEIVED		    EQU 0x2A	; Fleg conforme echo ha estat rebut
TOTAL_CORRECT		    EQU 0x2B
ENABLE_SPEAKER		    EQU 0x2C
COUNT_SPEAKER_TIMES	    EQU 0x2D
SPEAKER_LEVEL		    EQU 0x2E	
COUNT_3S_STARTED	    EQU 0x2F
DISABLE_COUNT_3S_STARTED    EQU 0x30
		
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; -----------------------  INTERRUPTS   ----------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
HIGH_RSI
    BTFSC   INTCON, TMR0IF, 0
    CALL    RSI_TIMER0
    
    RETFIE  FAST
    
RSI_TIMER0
    BCF	    INTCON, TMR0IF, 0
    CALL    RESET_TIMER0
    
    BTFSS   PORTC, 7, 0		    ; Comprova si StartGame = 1
    RETURN
        
    CALL    HANDLE_RSI_ECHO   
    CALL    HANDLE_RSI_SPEAKER
    CALL    HANDLE_RSI_NOTE
    CALL    HANDLE_RSI_SERVO

    RETURN
 
;========== RSI ECHO ================
HANDLE_RSI_ECHO
    BTFSS   PORTC, 5, 0			    ; Echo actiu?
    RETURN				    ; no -> return
    
    INCF    COUNT_N_RSI_ECHO, F, 0	    ; si -> echo + 1 -> 100uS de echo = 1,72CM
    SETF    ECHO_RECEIVED, 0		    ; flag començat a rebre echo
    RETURN

;========== RSI SPK ================
HANDLE_RSI_SPEAKER
    BTFSS   ENABLE_SPEAKER, 0		; speaker enabled?
    RETURN				; no -> return
    
    ; Comprovar si hem arribat al període de canvi
    MOVF    NOTE_PERIOD, W, 0          ; Carrega el valor del període
    SUBWF   COUNT_SPEAKER_TIMES, W, 0  ; Resta comptador al període
    BTFSS   STATUS, Z, 0               ; Si no és 0, continua incrementant
    GOTO    INCREMENT_COUNTER

    ; Si el comptador arriba al període, canvia el nivell del senyal
    GOTO    CHANGE_SPEAKER_LEVEL

INCREMENT_COUNTER
    INCF    COUNT_SPEAKER_TIMES, F, 0  ; Incrementa comptador
    RETURN

CHANGE_SPEAKER_LEVEL
    ; Comprova l'estat actual de SPEAKER_LEVEL per alternar el senyal
    BTFSC   SPEAKER_LEVEL, 0           ; Si SPEAKER_LEVEL = 1, baixa
    GOTO    SPEAKER_LOW

    ; Si SPEAKER_LEVEL = 0, puja
    BTFSS   SPEAKER_LEVEL, 0
    GOTO    SPEAKER_HIGH
    RETURN

SPEAKER_HIGH
    BSF     LATC, 4, 0                 ; Activa el pin de l'altaveu (HIGH)
    SETF    SPEAKER_LEVEL, 0           ; Marca l'estat com HIGH
    CLRF    COUNT_SPEAKER_TIMES, 0     ; Reinicia el comptador
    RETURN

SPEAKER_LOW
    BCF     LATC, 4, 0                 ; Desactiva el pin de l'altaveu (LOW)
    CLRF    SPEAKER_LEVEL, 0           ; Marca l'estat com LOW
    CLRF    COUNT_SPEAKER_TIMES, 0     ; Reinicia el comptador
    RETURN
 
    
;========== RSI NOTE ================
HANDLE_RSI_NOTE
    CALL    RSI_COUNT_500mS
    CALL    RSI_COUNT_3S
    CALL    RSI_COUNT_LENGTH
    RETURN
    
    
RSI_COUNT_500mS
    BTFSC   FLAG_500mS, 0		; Flag 500mS = 0?
    RETURN				; si -> return
    
    ; Resta a la part baixa
    DECFSZ  COUNT_500mS_LOW, F, 0	; count low = 0?
    GOTO    END_RSI_COUNT_500mS		; no -> return
    
    ; Si part baixa arriba a zero, resta a la part alta
    DECFSZ  COUNT_500mS_HIGH, F, 0	; si -> restar a part alta i reset low
    GOTO    RESET_COUNT_500mS_LOW       ; Si part alta no és zero, reinicia part baixa

    ; Si part alta també arriba a zero, activa el flag
    SETF    FLAG_500mS, 0		; activar flag 500mS
    CLRF    FLAG_COUNT_LENGTH_STARTED, 0
    GOTO    END_RSI_COUNT_500mS

RESET_COUNT_500mS_LOW
    MOVLW   .255                    ; Reinicia la part baixa
    MOVWF   COUNT_500mS_LOW, 0

END_RSI_COUNT_500mS
    RETURN
    
 
RSI_COUNT_3S
    BTFSC   DISABLE_COUNT_3S_STARTED, 0, 0
    RETURN
    
    BTFSC   FLAG_3S, 0
    RETURN
    
    ; Resta a la part baixa
    DECFSZ  COUNT_3S_LOW, F, 0
    GOTO    END_RSI_COUNT_3S
    
    ; Si part baixa arriba a zero, resta a la part alta
    DECFSZ  COUNT_3S_HIGH, F, 0
    GOTO    RESET_COUNT_3S_LOW         ; Si part alta no és zero, reinicia part baixa

    ; Si part alta també arriba a zero, activa el flag
    SETF    FLAG_3S, 0
    GOTO    END_RSI_COUNT_3S

RESET_COUNT_3S_LOW
    MOVLW   .255                    ; Reinicia la part baixa
    MOVWF   COUNT_3S_LOW, 0
    GOTO    END_RSI_COUNT_3S

END_RSI_COUNT_3S
    RETURN    
    
    
RSI_COUNT_LENGTH
    BTFSS   FLAG_500mS, 0	    ; flag 500mS = 1?
    RETURN			    ; no -> return
    
    ; Resta a la part baixa
    DECFSZ  COUNT_LENGTH_LOW, F, 0  ; si -> comptar -> count = 0?
    GOTO    END_RSI_COUNT_LENGTH    ; no -> return
    
    ; Si part baixa arriba a zero, resta a la part alta
    DECFSZ  COUNT_LENGTH_HIGH, F, 0 ; si -> count_high = 0?
    GOTO    RESET_COUNT_LENGTH_LOW  ; Si part alta no és zero, reinicia part baixa

    ; Si part alta també arriba a zero, activa el flag
    SETF    FLAG_COUNT_LENGTH_FINISHED, 0
    GOTO    END_RSI_COUNT_LENGTH

RESET_COUNT_LENGTH_LOW
    MOVLW   .255                    ; Reinicia la part baixa
    MOVWF   COUNT_LENGTH_LOW, 0
    GOTO    END_RSI_COUNT_LENGTH

END_RSI_COUNT_LENGTH
    RETURN
 
    
;========== RSI SERVO ================  
HANDLE_RSI_SERVO
    ; Comprova si estem comptant els 17 ms
    BTFSC   ENABLE_COUNT_17mS, 0
    GOTO    COUNT_RSI_17mS

    ; Si el cicle ha acabat, torna a començar
    BTFSS   SERVO_ENDED_COUNT, 0
    RETURN

    MOVLW   .150
    MOVWF   COUNT_17mS, 0
    SETF    ENABLE_COUNT_17mS, 0
    RETURN

COUNT_RSI_17mS
    ; Decrementa el comptador de 17 ms
    DECFSZ  COUNT_17mS, F, 0
    GOTO    END_SERVO

    ; Si arriba a 0, marca el final del cicle
    CLRF    ENABLE_COUNT_17mS, 0
    SETF    SERVO_ENDED_COUNT, 0
    RETURN

END_SERVO
    RETURN
 
    
;========== RSI AUX ================
ENABLE_INTERRUTS
    BSF	    INTCON, GIE, 0	    ; Enable interrupts
    BSF	    INTCON, PEIE, 0	    ; Enable interrupts
    RETURN
    
DISABLE_INTERRUPTS
    BCF	    INTCON, GIE, 0	    ; Disable interrupts
    BCF	    INTCON, PEIE, 0	    ; Disable interrupts
    RETURN
    
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; -----------------------  CONFIGURATIONS  -------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
CONFIG_PORTS 
    ; INPUTS
    BSF	    TRISB, 0, 0	    ; Duration[0]
    BSF	    TRISB, 1, 0	    ; Duration[1]
    BSF	    TRISB, 2, 0	    ; Note[0]
    BSF	    TRISB, 3, 0	    ; Note[1]
    BSF	    TRISB, 4, 0	    ; Note[2]
    BSF	    TRISB, 5, 0	    ; Note[3]
    
    BSF	    TRISC, 6, 0	    ; NewNote
    BSF	    TRISC, 7, 0	    ; StartGame
    BSF	    TRISC, 5, 0	    ; Echo
    
    ; OUTPUTS
    BCF	    TRISA, 3, 0	    ; AnswerIncorrect
    BCF	    TRISA, 4, 0	    ; AnswerCorrect
    BCF	    TRISA, 5, 0	    ; Ack
    
    BCF	    TRISC, 0, 0	    ; GameScore
    BCF	    TRISC, 1, 0	    ; Length[1]
    BCF	    TRISC, 2, 0	    ; Length[0]
    BCF	    TRISC, 3, 0	    ; Trigger
    BCF	    TRISC, 4, 0	    ; Speaker

    CLRF    TRISD, 0	    ; CurrentNote
    
    ; Inicialitzar a 0 tots els outputs
    BCF	    LATA, 3, 0
    BCF	    LATA, 4, 0
    BCF	    LATA, 5, 0
    BCF	    LATC, 0, 0
    BCF	    LATC, 1, 0
    BCF	    LATC, 2, 0
    BCF	    LATC, 3, 0
    BCF	    LATC, 4, 0
    SETF    LATD, 0
    RETURN
    
CONFIG_INTERRUPTS
    BCF	    RCON, IPEN, 0	    ; Disable priority
    BSF	    INTCON, GIE, 0	    ; Enable interrupts
    BSF	    INTCON, PEIE, 0	    ; Enable interrupts
    BSF	    INTCON, TMR0IE, 0	    ; Enable interrupt T0
    BSF	    INTCON2, RBPU, 0	    ; Pull ups disabled
    RETURN

CONFIG_TIMER0
    BCF	    T0CON, TMR0ON, 0	    ; Disable Timer0  
    BCF	    T0CON, T08BIT, 0	    ; Timer0 16 bit counter
    BCF	    T0CON, T0CS, 0
    BCF	    T0CON, T0SE, 0
    BSF	    T0CON, PSA, 0	    ; Disable prescaler
    CALL    RESET_TIMER0
    CALL    ENABLE_TIMER0	    ; Enable Timer0
    RETURN
 
    
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;------------------------------     TIMER0    ----------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;   Fosc = 40Mhz    Tins = 4/40Mhz = 100ns
;   1000inst -> 100nS x 1000instr = 100uS -> carreguem 65535 - 1000
RESET_TIMER0
    MOVLW   HIGH(.64536)
    MOVWF   TMR0H, 0
    MOVLW   LOW(.64536)
    MOVWF   TMR0L, 0
    RETURN
    
ENABLE_TIMER0
    BSF	    T0CON, TMR0ON, 0
    RETURN
 
DISABLE_TIMER0
    BCF	    T0CON, TMR0ON, 0
    RETURN
 
    
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;------------------------------   SERVO     ------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;  
HANDLE_SERVO
    BTFSS   SERVO_ENDED_COUNT, 0, 0
    RETURN
    
    BSF	    LATC, 0, 0
    CALL    WAIT_500uS
    CALL    WAIT_SERVO_UP
    
    BCF	    LATC, 0, 0
    CALL    WAIT_SERVO_DOWN
    
    SETF    ENABLE_COUNT_17mS, 0
    CLRF    SERVO_ENDED_COUNT, 0
    
    RETURN
    
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;------------------------------   SPEAKER     ----------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;    
SET_DO
    MOVLW   .19                  ; Mig període de DO (19 unitats de 100 µs)
    MOVWF   NOTE_PERIOD, 0
    SETF    ENABLE_SPEAKER, 0
    RETURN

SET_RE
    MOVLW   .17                  ; Mig període de RE (17 unitats de 100 µs)
    MOVWF   NOTE_PERIOD, 0
    SETF    ENABLE_SPEAKER, 0
    RETURN

SET_MI
    MOVLW   .15                  ; Mig període de MI (15 unitats de 100 µs)
    MOVWF   NOTE_PERIOD, 0
    SETF    ENABLE_SPEAKER, 0
    RETURN

SET_FA
    MOVLW   .14                  ; Mig període de FA (14 unitats de 100 µs)
    MOVWF   NOTE_PERIOD, 0
    SETF    ENABLE_SPEAKER, 0
    RETURN

SET_SOL
    MOVLW   .12                  ; Mig període de SOL (12 unitats de 100 µs)
    MOVWF   NOTE_PERIOD, 0
    SETF    ENABLE_SPEAKER, 0
    RETURN

SET_LA
    MOVLW   .11                  ; Mig període de LA (11 unitats de 100 µs)
    MOVWF   NOTE_PERIOD, 0
    SETF    ENABLE_SPEAKER, 0
    RETURN

SET_SI
    MOVLW   .10                  ; Mig període de SI (10 unitats de 100 µs)
    MOVWF   NOTE_PERIOD, 0
    SETF    ENABLE_SPEAKER, 0
    RETURN


PROCESS_TIME_TO_SUBTRACT_SERVO
    CLRF    TBLPTRH, 0
    MOVLW   0x39
    MOVWF   TBLPTRL, 0
    
    MOVF    N_NOTES, 0
    ADDWF   TBLPTRL, F, 0
    
    TBLRD*
    
    MOVFF   TABLAT, TIME_TO_SUBTRACT_SERVO
    RETURN
 
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;--------------------     SAVE INFO RAM     ------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
HANDLE_NEW_NOTE
    ; Inici del punter de la RAM a 0x0100
    ; 1byte a la RAM -> [X, X, Note3, Note 2, Note1 , Note0, Duration1, Duration0]
    BCF     LATA, 5, 0               ; Ack = 0

    BTFSC   PORTC, 6, 0              ; Comprova si NewNote = 0
    RETURN                           ; Si no hi ha nota nova, surt
    
    MOVF    PORTB, W, 0              ; Llegeix el valor de PORTB
    ANDLW   b'00111111'              ; Mask dels 6lsb
    
    MOVWF   POSTINC0, 0              ; Desa a la RAM i incrementa el punter
    
    INCF    N_NOTES, F, 0            ; Incrementa el nombre de notes guardades
    
    BSF     LATA, 5, 0               ; Ack = 1
    
    CALL    WAIT_1mS
    
    
    RETURN

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;-----------------     SHOW 7SEG AND LENGTH LEDS   -----------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;    
SHOW_RAM_INFO	    ; Cal posar el punter a l'@ on es vol mostrar la info
    ;	[X, X, NOTE3, NOTE2, NOTE1, NOTE0, DURATION1, DURATION0]  
    MOVF    INDF0, W, 0			    ; INDF0 -> W
    MOVWF   RAM_VALUE_AUX1, 0		    ; W -> RAM_VALUE_AUX1
    MOVWF   RAM_VALUE_AUX2, 0		    ; W -> RAM_VALUE_AUX2
    

    ; Extreure CurrentNote[7..0]
    ANDLW   b'00111100'			    ; Fem mask de 00111100 per extraure Note
    RRNCF   RAM_VALUE_AUX1, W, 0	    ; Rotar 1 bit a la dreta
    MOVWF   RAM_VALUE_AUX1, 0		    ; W -> RAM_VALUE_AUX1
    RRNCF   RAM_VALUE_AUX1, W, 0	    ; Rotar 1 bit a la dreta
    MOVWF   CURRENT_NOTE, 0		    ; W -> CURRENT_NOTE
    CALL    SHOW_CURRENT_NOTE_7SEG	    ; Mostra CurrentNote[7..0]
    MOVWF   PORTD, 0
    
    ; Extreure Length[1..0]
    MOVF    RAM_VALUE_AUX2, W, 0	    ; RAM_VALUE -> W
    ANDLW   b'00000011'			    ; Fem mask de 00000011 per extraure Duration 
    MOVWF   LENGTH, 0
    CALL    SHOW_LENGTH_LEDS		    ; Mostra Length[1..0]
    
    RETURN

SHOW_CURRENT_NOTE_7SEG
    ; [0, 0, 0, 0, NOTE3, NOTE2, NOTE1, NOTE0]
    MOVLW   b'00001111'            ; Màscara per extreure els 4 LSB
    ANDWF   CURRENT_NOTE, W
    MOVWF   CURRENT_NOTE, 0

    ; Comparar amb 0
    MOVLW   0x00                 ; Carrega 0 a WREG
    SUBWF   CURRENT_NOTE, W, 0   ; W = CURRENT_NOTE - WREG
    BTFSC   STATUS, Z            ; Si Z (zero) és 1, CURRENT_NOTE és 0
    GOTO    DISPLAY_0            ; Salta a la rutina per mostrar 0

    ; Comparar amb 1
    MOVLW   0x01
    SUBWF   CURRENT_NOTE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISPLAY_1

    ; Comparar amb 2
    MOVLW   0x02
    SUBWF   CURRENT_NOTE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISPLAY_2
    
    ; Comparar amb 3
    MOVLW   0x03
    SUBWF   CURRENT_NOTE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISPLAY_3

    ; Comparar amb 4
    MOVLW   0x04
    SUBWF   CURRENT_NOTE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISPLAY_4

    ; Comparar amb 5
    MOVLW   0x05
    SUBWF   CURRENT_NOTE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISPLAY_5

    ; Comparar amb 6
    MOVLW   0x06
    SUBWF   CURRENT_NOTE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISPLAY_6

    RETURN

DISPLAY_0
    MOVLW   b'10000010'          ; Patró per al número 0
    MOVWF   PORTD, 0
    RETURN

DISPLAY_1
    MOVLW   b'11001111'          ; Patró per al número 1
    MOVWF   PORTD, 0
    RETURN

DISPLAY_2
    MOVLW   b'10010001'          ; Patró per al número 2
    MOVWF   PORTD, 0
    RETURN

DISPLAY_3
    MOVLW   b'10000101'          ; Patró per al número 3
    MOVWF   PORTD, 0
    RETURN

DISPLAY_4
    MOVLW   b'11001100'          ; Patró per al número 4
    MOVWF   PORTD, 0
    RETURN

DISPLAY_5
    MOVLW   b'10100100'          ; Patró per al número 5
    MOVWF   PORTD, 0
    RETURN

DISPLAY_6
    MOVLW   b'10100000'          ; Patró per al número 6
    MOVWF   PORTD, 0
    RETURN

SHOW_LENGTH_LEDS
    ; [0, 0, 0, 0, 0, 0, DURATION1, DURATION0]
    BTFSC   LENGTH, 0, 0
    BSF	    LATC, 2, 0		    ; Length[0] = 1
    BTFSS   LENGTH, 0, 0
    BCF	    LATC,2 , 0		    ; Length[0] = 0
    
    BTFSC   LENGTH, 1, 0
    BSF	    LATC,1 , 0		    ; Length[1] = 1
    BTFSS   LENGTH, 1, 0
    BCF	    LATC,1 , 0		    ; Length[1] = 0
    RETURN
 
   
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;----------------------     WAITS  ---------------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-; 
; 10uS = 100instr x 100nS
WAIT_10uS			    ; 2+1+1+2 + 3x = 100instr -> x = 31		2
    MOVLW   .31			    ; Carrega 100 (nombre d'instruccions)       1	
    MOVWF   COUNT_10uS, 0	    ; Guarda a la variable COUNT		1
WAIT_LOOP_10uS
    DECFSZ  COUNT_10uS, F, 0	    ; Decrementa COUNT i salta si arriba a 0	3
    GOTO    WAIT_LOOP_10uS	    ; Continua al bucle				0
    
    RETURN			    ;						2

WAIT_500uS			; 2
    MOVLW   .50			; 1
    MOVWF   COUNT_500uS, 0	; 1
WAIT_LOOP_500uS
    CALL    WAIT_10uS		; 10uS
    DECFSZ  COUNT_500uS, F, 0	; 3
    GOTO    WAIT_LOOP_500uS	; 0
    
    RETURN			; 2
    
WAIT_1mS
    MOVLW   .100
    MOVWF   COUNT_1mS, 0
WAIT_LOOP_1mS
    CALL    WAIT_10uS
    DECFSZ  COUNT_1mS, F, 0
    GOTO    WAIT_LOOP_1mS
    
    RETURN
    
WAIT_17mS
    MOVLW   .17
    MOVWF   COUNT_17mS, 0
WAIT_LOOP_17mS
    CALL    WAIT_1mS
    DECFSZ  COUNT_17mS, F, 0
    GOTO    WAIT_LOOP_17mS
    
    RETURN
 

WAIT_SERVO_UP
    MOVFF   TOTAL_CORRECT, COUNT_SERVO_2
WAIT_LOOP_SERVO_UP_2
    MOVFF   TIME_TO_SUBTRACT_SERVO, COUNT_SERVO
WAIT_LOOP_SERVO_UP
    CALL    WAIT_10uS
    DECFSZ  COUNT_SERVO, F, 0
    GOTO    WAIT_LOOP_SERVO_UP
    DECFSZ  COUNT_SERVO_2, F, 0
    GOTO    WAIT_LOOP_SERVO_UP_2

    RETURN
    
WAIT_SERVO_DOWN
    MOVF    TOTAL_CORRECT, 0
    SUBWF   N_NOTES, W, 0
    MOVWF   COUNT_SERVO_2, 0
WAIT_LOOP_SERVO_DOWN_2
    MOVFF   TIME_TO_SUBTRACT_SERVO, COUNT_SERVO
WAIT_LOOP_SERVO_DOWN
    CALL    WAIT_10uS
    DECFSZ  COUNT_SERVO, F, 0
    GOTO    WAIT_LOOP_SERVO_UP
    DECFSZ  COUNT_SERVO_2, F, 0
    GOTO    WAIT_LOOP_SERVO_DOWN_2

    RETURN
 
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;----------------------     ULTRASONS HANDLING  --------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-; 
HANDLE_ULTRASONS
    ; Si el senyal TRIGGER_SENT ja està actiu, no tornar a enviar
    BTFSC   TRIGGER_SENT, 0		    ; Trigger enviat? 
    CALL    CHECK_ECHO_RECEIVED		    ; si -> mirar echo
    
    ; Comprovar si el pin de l'eco ha tornat a 0
    BTFSC   PORTC, 5, 0			    ; echo es 0?
    RETURN				    ; si -> return
					    
    MOVFF   COUNT_N_RSI_ECHO, DISTANCE	    ; no -> comptar temps de echo
    
    CLRF    ECHO_RECEIVED, 0		    ; Posa a 0 la flag de rebut
    CLRF    TRIGGER_SENT, 0		    ; Permet un nou senyal de trigger
    CLRF    COUNT_N_RSI_ECHO, 0		    ; reset counter temps echo

    BSF     LATC, 3, 0			    ; Activa Trigger
    CALL    WAIT_10uS			    ; Espera 10uS
    BCF     LATC, 3, 0			    ; Desactiva Trigger
    
    SETF    TRIGGER_SENT, 0		    ; Marca que s'ha enviat el trigger
    
ECHO_STILL_NOT_RECEIVED
    RETURN

CHECK_ECHO_RECEIVED
    ; Comprovar si el senyal ECHO_RECEIVED ha acabat
    BTFSS   ECHO_RECEIVED, 0
    GOTO    ECHO_STILL_NOT_RECEIVED
    RETURN   

    
PROCESS_DISTANCE
    ; Inicialització
    ; 1 COUNT_N_RSI_ECHO EQUIVAL A 1,7CM
    MOVLW   .4                 
    SUBWF   DISTANCE, W, 0   
    BTFSC   STATUS, Z, 0
    GOTO    DISTANCE_7

    MOVLW   .8
    SUBWF   DISTANCE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISTANCE_14

    MOVLW   .12
    SUBWF   DISTANCE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISTANCE_21
    
    MOVLW   .16
    SUBWF   DISTANCE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISTANCE_28

    MOVLW   .20
    SUBWF   DISTANCE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISTANCE_35

    MOVLW   .24
    SUBWF   DISTANCE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISTANCE_42

    MOVLW   .26
    SUBWF   DISTANCE, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    DISTANCE_50
    
    GOTO    END_PROCESS_DISTANCE

DISTANCE_7
    CALL    SET_DO
    MOVLW   0x00
    MOVWF   NOTE_LISTENED, 0
    GOTO    END_PROCESS_DISTANCE

DISTANCE_14
    CALL    SET_RE
    MOVLW   0x01
    MOVWF   NOTE_LISTENED, 0
    GOTO    END_PROCESS_DISTANCE

DISTANCE_21
    CALL    SET_MI
    MOVLW   0x02
    MOVWF   NOTE_LISTENED, 0
    GOTO    END_PROCESS_DISTANCE

DISTANCE_28
    CALL    SET_FA
    MOVLW   0x03
    MOVWF   NOTE_LISTENED, 0
    GOTO    END_PROCESS_DISTANCE

DISTANCE_35
    CALL    SET_SOL
    MOVLW   0x04
    MOVWF   NOTE_LISTENED, 0
    GOTO    END_PROCESS_DISTANCE

DISTANCE_42
    CALL    SET_LA
    MOVLW   0x05
    MOVWF   NOTE_LISTENED, 0
    GOTO    END_PROCESS_DISTANCE

DISTANCE_50
    CALL    SET_SI
    MOVLW   0x06
    MOVWF   NOTE_LISTENED, 0
    GOTO    END_PROCESS_DISTANCE

END_PROCESS_DISTANCE
    RETURN


;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;----------------------     NOTE HANDLING  -------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-; 
HANDLE_NOTE
    BTFSC   FLAG_3S, 0			; HAN PASAT 3S?
    GOTO    PROCESS_INCORRECT_ANSWER	; si -> incorrect answer
    
    BTFSS   COUNT_3S_STARTED, 0, 0	; no -> han començat a comptar els 3s?
    GOTO    START_COUNT_3S		; no -> comença a comptar 3s
		
RESUME_HANDLE_NOTE			; si -> segueix 
    BTFSC   FLAG_500mS, 0		; han pasat 500ms amb la nota correcta?
    GOTO    START_COUNT_NOTE		; si -> comença a comptar
    
    MOVF    NOTE_LISTENED, W, 0		; no -> mirar si estas a la nota correcta
    SUBWF   CURRENT_NOTE, W, 0		; CURRENT_NOTE = NOTE_LISTENED?
    BTFSC   STATUS, Z, 0	
    GOTO    END_HANDLE_NOTE		; no -> end 
    CALL    RESET_COUNT_500mS		; si -> reset al couter de 500 i comença
    
END_HANDLE_NOTE    
    RETURN

START_COUNT_3S 
    BTFSC   DISABLE_COUNT_3S_STARTED, 0, 0  ; counter 3s disabled?
    GOTO    RESUME_HANDLE_NOTE		    ; si -> resume sense comptar 3s
    
    CALL    RESET_COUNT_3S		    ; no -> comença a comptar 3s 
    SETF    COUNT_3S_STARTED, 0		    ; indica que ha començat a comptar 3s
    
    GOTO    END_HANDLE_NOTE		    ; return
    
START_COUNT_NOTE
    SETF    DISABLE_COUNT_3S_STARTED, 0	    ; disable count 3s
    
    BTFSC   FLAG_COUNT_LENGTH_STARTED, 0    ; count length started?
    GOTO    CHECK_ANSWER		    ; si -> mirar answer
    
    SETF    FLAG_COUNT_LENGTH_STARTED, 0    ; no -> començar
    CLRF    ERROR_MADE, 0		    ; netejar flag error
    
    BCF	    LATA, 4, 0			    ; apagar led
    BCF	    LATA, 3, 0			    ; apagar led
    
    MOVLW   .1				    ; mirar si durada es 1s
    SUBWF   LENGTH, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    SET_COUNT_DURATION_1
    
    MOVLW   .2				    ; mirar si durada es 2s
    SUBWF   LENGTH, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    SET_COUNT_DURATION_2
    
    MOVLW   .3				    ; mirar si durada es 3s
    SUBWF   LENGTH, W, 0
    BTFSC   STATUS, Z, 0
    GOTO    SET_COUNT_DURATION_3
    
    GOTO    END_HANDLE_NOTE		    ; return
    
SET_COUNT_DURATION_1	; 1S
    MOVLW   HIGH(.10000) 
    MOVWF   COUNT_LENGTH_HIGH, 0
    MOVLW   LOW(.10000)
    MOVWF   COUNT_LENGTH_LOW, 0
    GOTO    END_HANDLE_NOTE
    
SET_COUNT_DURATION_2	; 2S
    MOVLW   HIGH(.20000)
    MOVWF   COUNT_LENGTH_HIGH, 0
    MOVLW   LOW(.20000)
    MOVWF   COUNT_LENGTH_LOW, 0
    GOTO    END_HANDLE_NOTE
    
SET_COUNT_DURATION_3	; 3S
    MOVLW   HIGH(.30000)
    MOVWF   COUNT_LENGTH_HIGH, 0
    MOVLW   LOW(.30000)
    MOVWF   COUNT_LENGTH_LOW, 0
    GOTO    END_HANDLE_NOTE

CHECK_ANSWER
    MOVF    NOTE_LISTENED, W, 0
    SUBWF   CURRENT_NOTE, W, 0	    
    BTFSS   STATUS, Z, 0	    ; CURRENT_NOTE = NOTE_LISTENED?
    SETF    ERROR_MADE, 0	    ; no -> flag error = 1

    GOTO    END_HANDLE_NOTE	    ; si -> return
    
RESET_COUNT_500mS
    MOVLW   HIGH(.5000)
    MOVWF   COUNT_500mS_HIGH, 0
    
    MOVLW   LOW(.5000)
    MOVWF   COUNT_500mS_LOW, 0
    
    CLRF    FLAG_500mS, 0 
    RETURN
    
RESET_COUNT_3S
    MOVLW   HIGH(.30000)
    MOVWF   COUNT_3S_HIGH, 0
    
    MOVLW   LOW(.30000)
    MOVWF   COUNT_3S_LOW, 0
    
    CLRF    FLAG_3S, 0 
    RETURN
    
    
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;----------------------     ANSWER HANDLING  -----------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;   
HANDLE_ANSWER
    BTFSS   FLAG_COUNT_LENGTH_FINISHED, 0   ; counter length acabat?
    RETURN				    ; no -> return
        
    BTFSS   ERROR_MADE, 0		    ; si -> hi ha hagut error?
    GOTO    PROCESS_CORRECT_ANSWER	    ; no -> validar correcte 
    
    GOTO    PROCESS_INCORRECT_ANSWER	    ; si -> validar incorrecte
  
END_HANDLE_ANSWER
    CLRF    ERROR_MADE, 0
    CLRF    FLAG_COUNT_LENGTH_STARTED, 0
    CLRF    FLAG_COUNT_LENGTH_FINISHED, 0
    
    CALL    RESET_COUNT_500mS
    CALL    RESET_COUNT_3S
    
    INCF    N_NOTES_REPRODUIDES, F, 0
        
    INCF    FSR0L, F, 0			; Increment al punter de la RAM
    RETURN
    

PROCESS_CORRECT_ANSWER
    BSF	    LATA, 4, 0
    BCF	    LATA, 3, 0
    INCF    TOTAL_CORRECT, F, 0
    GOTO    END_HANDLE_ANSWER
    
PROCESS_INCORRECT_ANSWER  
    BCF	    LATA, 4, 0
    BSF	    LATA, 3, 0
    GOTO    END_HANDLE_ANSWER

    
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;----------------------------     INITS     ------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
INIT_VARS_MAIN
    CLRF    N_NOTES, 0		; Init N_NOTES a 0
    CLRF    CURRENT_NOTE, 0	; Init CurrentNote a 0
    RETURN
    
INIT_VARS_START_GAME
    CLRF    N_NOTES_REPRODUIDES, 0
    CLRF    NOTE_PERIOD, 0
    SETF    DISTANCE, 0
    CLRF    COUNT_SPEAKER_TIMES, 0
    CLRF    SPEAKER_LEVEL, 0
    CLRF    TRIGGER_SENT, 0
    CLRF    ECHO_RECEIVED, 0
    CLRF    ENABLE_SPEAKER, 0
    SETF    NOTE_LISTENED, 0
    CLRF    FLAG_COUNT_LENGTH_STARTED, 0
    CLRF    FLAG_COUNT_LENGTH_FINISHED, 0
    CLRF    ERROR_MADE, 0
    CLRF    TOTAL_CORRECT, 0
    CLRF    ENABLE_COUNT_17mS, 0
    SETF    SERVO_ENDED_COUNT, 0
    CLRF    TIME_TO_SUBTRACT_SERVO, 0
    CLRF    DISABLE_COUNT_3S_STARTED, 0
    CLRF    COUNT_3S_STARTED, 0
    CALL    RESET_COUNT_500mS
    CALL    RESET_COUNT_3S
    
    RETURN
    
INIT_RAM
    ; INIT PUNTER RAM -> 0x0100
    MOVLW   0x01
    MOVWF   FSR0H, 0
    MOVLW   0x00
    MOVWF   FSR0L, 0
    
    RETURN
    
INIT_SERVO
    BSF	    LATC, 0, 0
    CALL    WAIT_500uS
    
    BCF	    LATC, 0, 0
    CALL    WAIT_17mS
    CALL    WAIT_500uS
    CALL    WAIT_1mS
    CALL    WAIT_1mS
        
    RETURN
    
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;----------------------     START GAME  ----------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-; 
START_GAME
    MOVF    N_NOTES_REPRODUIDES, W, 0    ; Carrega el nombre de notes reproduïdes
    SUBWF   N_NOTES, W, 0                ; Compara amb el total de notes
    BTFSC   STATUS, Z, 0                 ; Si són iguals, final del joc
    GOTO    GAME_OVER

    CALL    SHOW_RAM_INFO
    CALL    HANDLE_ULTRASONS
    CALL    PROCESS_DISTANCE
    CALL    HANDLE_NOTE
    CALL    HANDLE_ANSWER 
RESUME_GAME
    RETURN
 
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;----------------------     START GAME LOOP    ---------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;    
HANDLE_START_GAME
    BTFSS   PORTC, 7, 0			    ; Comprova si StartGame = 1
    RETURN
        
    CALL    INIT_VARS_START_GAME	    ; init de totes les vars
    CALL    INIT_RAM			    ; init punters ram
    CALL    PROCESS_TIME_TO_SUBTRACT_SERVO  ; init valor flash

START_GAME_LOOP				    ; Main Loop un cop començat Joc
    
    CALL    HANDLE_SERVO		    ; Posa el servo a 0 graus
    CALL    START_GAME			    ; Processa el joc
    
    GOTO    START_GAME_LOOP


;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;----------------------     GAME OVER    ---------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;      
GAME_OVER
    CALL    HANDLE_SERVO
    CLRF    ENABLE_SPEAKER, 0
    MOVLW   b'11111101'
    MOVWF   PORTD, 0
    BCF	    LATC, 1, 0
    BCF	    LATC, 2, 0
    GOTO    GAME_OVER      

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; ----------------------------   MAIN    ---------------------------------------;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
MAIN
    CALL    CONFIG_PORTS
    CALL    CONFIG_INTERRUPTS
    CALL    CONFIG_TIMER0
    
    CALL    INIT_VARS_MAIN
    CALL    INIT_RAM
    
LOOP
    CALL    HANDLE_NEW_NOTE
    CALL    INIT_SERVO
    CALL    HANDLE_START_GAME
    
    GOTO LOOP
    
    END
