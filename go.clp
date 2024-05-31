; Definición basica de tablero y jugador

(defglobal ?*tamano* = 0)

(deftemplate tablero
    (slot id)
    (slot padre)
    (slot nivel)
    (multislot matriz)
)

(deftemplate jugador
    (slot id)
    (slot tipo)
    (slot color)
    (slot puntos)
    (slot activo)
)

; //////////////////////// Funciones (meter debajo de uwu_grupo)

; Las siguientes funciones nos las daban en egela. Están modificadas para que se adapten a nuestro juego. Sirven para representar visualmente el tablero
; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(deffunction generarLineas (?x)
  (printout t crlf)
  (printout t "      |")
  (loop-for-count ?x
    (printout t "-----|")
  )
  (printout t crlf)
)

(deffunction generarLineas2 (?x)
  (printout t " ")
  (loop-for-count ?x
    (printout t "     |")
  )
  (printout t "     |")
  (printout t crlf)
)

; TODO corregir esta funcion para que se adecúe correctamente a nuestro juego
(deffunction imprimir ($?mapeo)
    (printout t crlf)
    (printout t crlf)
    (loop-for-count (?i 0 ?*tamano*) do
      (if (= ?i 0) then
      (printout t "       ")
      else
      (printout t "  "?i "   "))
    )
    (generarLineas ?*tamano*)
    (loop-for-count (?fila 1 ?*tamano* ) do
      (generarLineas2 ?*tamano*)
      (printout t "   " ?fila "  |" )
      (loop-for-count (?columna 1 ?*tamano*) do
            (bind ?contenido (nth$  (+ (* ?*tamano* (- ?fila 1)) ?columna) $?mapeo))
            (if (eq ?contenido b) then
                (printout t  "  B  |")
            )
            (if (eq ?contenido n) then
                (printout t  "  N  |")
            )
			(if (eq ?contenido 0) then
				(printout t "     |")
			)
      )
      (generarLineas ?*tamano*)
    )
)

; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


; Función para determinar si una posición está fuera de los límites del tablero
(deffunction fuera-de-tablero (?x ?y)
    (if (or (< ?x 1) (< ?y 1) (> ?x ?*tamano*) (> ?y ?*tamano*))
        then
        (return TRUE)
        else
        (return FALSE)
    )
)

(deffunction pos-a-coord (?pos)
    (bind ?x (mod (- ?pos 1) ?*tamano*)) ; Ajustar a índice basado en 1
    (bind ?y (+ 1 (div (- ?pos 1) ?*tamano*)))
    (return (create$ (+ ?x 1) ?y)) ; Ajustar la coordenada x para ser 1-basada
)


(deffunction coord-a-pos (?x ?y)
    (return (+ (* (- ?y 1) ?*tamano*) ?x))
)


(deffunction adyacente-a-oponente (?pos ?ultimoColor $?mapeo)
    (bind ?contenido (nth$ ?pos $?mapeo))
    (return (neq ?contenido ?ultimoColor))
)


; c     := color de las fichas que comen
(deffunction rodea (?grupo ?c $?mapeo)
    (bind $?acomer (create$))
    (foreach ?pos ?grupo
        ;(printout t "Comprobando si la posición " ?pos " está rodeada..." crlf)
        (bind ?coords (pos-a-coord ?pos))
        ;(printout t "Coordenadas de la posición: " ?coords crlf)
        (bind ?x (nth$ 1 ?coords))
        (bind ?y (nth$ 2 ?coords))
        
        ; Comprueba las posiciones adyacentes (norte, sur, este, oeste)
        (bind ?norte (if (not (fuera-de-tablero ?x (- ?y 1)))
                        then (coord-a-pos ?x (- ?y 1))
                        else f))
        (bind ?sur (if (not (fuera-de-tablero ?x (+ ?y 1)))
                      then (coord-a-pos ?x (+ ?y 1))
                      else f))
        (bind ?este (if (not (fuera-de-tablero (+ ?x 1) ?y))
                       then (coord-a-pos (+ ?x 1) ?y)
                       else f))
        (bind ?oeste (if (not (fuera-de-tablero (- ?x 1) ?y))
                       then (coord-a-pos (- ?x 1) ?y)
                       else f))
        ;(printout t "Coordenadas norte, sur, este, oeste: " ?norte " " ?sur " " ?este " " ?oeste crlf)
        
        ; Verifica si la ficha en la posición ?pos está rodeada por fichas del color aliado o por límites del tablero
        (if (and (or (eq ?norte f) (eq (nth$ ?norte $?mapeo) ?c))
                (or (eq ?sur f) (eq (nth$ ?sur $?mapeo) ?c))
                (or (eq ?este f) (eq (nth$ ?este $?mapeo) ?c))
                (or (eq ?oeste f) (eq (nth$ ?oeste $?mapeo) ?c)))
            then
                (bind $?acomer (create$ $?acomer ?pos))
        )
    )
    ;(printout t "Rodea: " ?rodea crlf)
    (if (eq (length$ $?acomer) 0)
        then
        (return FALSE)
        else
        (return $?acomer)
    )
)

; pos    := posición de la ultima ficha colocada
; c      := color de las fichas adyacentes que estás buscando
; $?mapeo:= tablero (basicamente)
(deffunction obtener-adyacentes (?pos ?c $?mapeo)
    (bind ?adyacentes (create$))   ; inicializamos 
    ;(printout t "Posición de la ficha colocada (antes de pasar por pos-a-coord): " ?pos crlf)
    (bind $?pos (pos-a-coord ?pos))
    ;(printout t "Posición de la ficha colocada (después de pasar por pos-a-coord): " $?pos crlf)
    (bind ?x (nth$ 1 $?pos))       ; obtenemos la coordenada x
    (bind ?y (nth$ 2 $?pos))       ; obtenemos la coordenada y
    (loop-for-count (?dy -1 1) do               ; creamos coordenadas locales para poder recorrer los adyacentes
        (loop-for-count (?dx -1 1) do           ; de forma sencilla
            (if (or (neq ?dx 0) (neq ?dy 0))    ; excluimos la posición actual 
                then
                (progn
                    (bind ?nx (+ ?x ?dx))       ; calculamos la posicion real del adyacente que estamos calculando
                    (bind ?ny (+ ?y ?dy))    
                    (if (not (fuera-de-tablero ?nx ?ny)) then   ; si no está fuera del tablero
                        ;(printout t "Coordenadas del adyacente con coordenadas locales " ?dx " " ?dy ": " ?nx " " ?ny crlf) 
                        (bind ?nPos (coord-a-pos ?nx ?ny))
                        ;(printout t "Posición real del adyacente: " ?nPos crlf)
                        (bind ?est (nth$ ?nPos $?mapeo))
                        (if (eq ?est ?c) then              ; si es del color del jugador
                            (bind ?adyacentes (create$ ?adyacentes ?nPos))  ; lo añadimos a la lista de adyacentes
                        )
                    else 
                        ;(printout t "Adyacente analizando fuera de tablero" crlf)
                    )
                )
            )
        )
    )
    (return ?adyacentes)
)


(deffunction grupo (?pos ?color1 $?mapeo)
    ; Inicializa el color del jugador y el oponente
    (if (eq ?color1 b) then
        (bind ?color2 n)
    else
        (bind ?color2 b)
    )

    ;(printout t "Obteniendo fichas enemigas adyacentes a la ficha colocada..." crlf)
    (bind ?grupoEnemigo (obtener-adyacentes ?pos ?color2 $?mapeo))
    ;(printout t "Adyacentes: " ?grupoEnemigo crlf)

    (if (neq (length$ ?grupoEnemigo) 0)
        then
        (progn
            ; Verifica si las fichas capturables están rodeadas por el grupo
            ;(printout t "Verificando si se come alguna ficha..." crlf)
            (bind ?res (rodea ?grupoEnemigo ?color1 $?mapeo))
            (if (neq ?res FALSE)
                then 
                    ;(printout t "Se debe(n) comer ficha(s)" crlf)
                    (return ?res)
                else 
                    ;(printout t "No se debe comer nada" crlf)
                    (return FALSE)
            )
        )
    )
    (return FALSE)
)



(deffunction comer (?pos ?c $?mapeo)
    ;(printout t "Entrando en grupo para ver si tocamos alguna ficha adyacente enemiga" crlf)
    (bind ?res (grupo ?pos ?c $?mapeo)) ; tengo que conseguir que res sea una lista de posiciones de fichas a eliminar
    (if (neq ?res FALSE) ; IF no es FALSE, entonces 
        then
        (progn
            ;(printout t "Se van a eliminar las fichas de las siguientes posiciones: " ?res crlf)
            (foreach ?p ?res
                (bind $?mapeo (replace$ $?mapeo ?p ?p 0))
            )
            (return $?mapeo)
        )
        else
        ;(printout t "No se elimina ninguna ficha" crlf)
        (return FALSE)
    )
)


; esta funcion tiene que comprobar si el ultimo movimiento jugado es legal. para ello seguramente
; usará la función grupo
;(deffunction verificar())
(deffunction verificar (?pos ?c $?mapeo)
    (if (eq ?c b)
    then 
        (bind ?c1 b)
        (bind ?c2 n)
    else 
        (bind ?c1 n)
        (bind ?c2 b)
    )

    (printout t "Entrando en RODEA con pos siendo: " ?pos crlf)
    (if (neq (rodea ?pos ?c2 $?mapeo) FALSE)
        then
        (return FALSE)
        else
        (return TRUE)
    )
)
; ///////////////////////////////////////////////////



; esta funcion tiene que evaluar si para el jugador dado por parámetro, le quedan movimientos legales
; para ello, para cada posicion del tablero que este libre, se tiene que ver si se puede colocar una ficha
; en esa posición (con cont > 0 valdría)
;(deffunction evaluar_fin())

; esta funcion tiene que comprobar si con el ultimo movimiento jugado, no existen mas movimientos legales
; para ello, se va a tener que comprobar cada vez que un jugador mueva, si el otro tiene al menos un movimiento
;(deffunction fin())

;///////////////////////////////////////////////////

;'Main' de la función

(defrule inicio
    (not (tablero))
=>  
    
    (printout t "Inserte el tamaño (A) del tablero (AxA). Escoga entre 4, 6 u 9): " crlf)
    (bind ?aux_t (read))

    ; inicializacion de tablero
    (bind ?*tamano* ?aux_t)
    (bind ?*tamano* ?aux_t)
    (if (eq ?aux_t 4) then
        (assert (tablero (id 0) (padre -1) (nivel 0) (matriz 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    )
    (if (eq ?aux_t 6) then
        (assert (tablero (id 0) (padre -1) (nivel 0) (matriz 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    )
    (if (eq ?aux_t 9) then
        (assert (tablero (id 0) (padre -1) (nivel 0) (matriz 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    ) 

    (printout t "Inserte tipo del jugador 1 (h/m) : " crlf)
    (bind ?tipo (read))
    (printout t "Inserte color del jugador 1 (b/n) : " crlf)
    (bind ?color (read))

    ; inicializacion de jugador1
    (if (eq ?tipo h) then
        (if (eq ?color b) then
            (assert (jugador (id 1) (tipo h) (color b) (puntos 0) (activo TRUE)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 1) (tipo h) (color n) (puntos 0) (activo TRUE)))
        )
    )
    (if (eq ?tipo m) then
        (if (eq ?color b) then
            (assert (jugador (id 1) (tipo m) (color b) (puntos 0) (activo TRUE)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 1) (tipo m) (color n) (puntos 0) (activo TRUE)))
        )
    )

    (printout t "Inserte tipo del jugador 2 (h/m) : " crlf)
    (bind ?tipo (read))
    (printout t "Inserte color del jugador 2 (b/n) : " crlf)
    (bind ?color (read))

    ; inicializacion de jugador2
    (if (eq ?tipo h) then
        (if (eq ?color b) then
            (assert (jugador (id 2) (tipo h) (color b) (puntos 0) (activo FALSE)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 2) (tipo h) (color n) (puntos 0) (activo FALSE)))
        )
    )
    (if (eq ?tipo m) then
        (if (eq ?color b) then
            (assert (jugador (id 2) (tipo m) (color b) (puntos 0) (activo FALSE)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 2) (tipo m) (color n) (puntos 0) (activo FALSE)))
        )
    )
)


(defrule mov
    ?j <- (jugador (id ?i) (tipo h) (color ?c) (puntos ?puntos) (activo TRUE))
    ?tab <- (tablero (matriz $?mapeo))
=>
    (bind ?aux TRUE)
    (while ?aux do
        (printout t (if (eq ?i 1) then "Jugador 1, " else "Jugador 2, ") "ingresa tu movimiento (x y):")
        (bind ?x (read))
        (bind ?y (read))
        (bind ?pos (+ (* ?*tamano* (- ?y 1)) ?x))
        (printout t "Posicion jugada: " ?pos crlf)

        (printout t "Verificando si el movimiento es válido..." crlf)
        (bind $?posm (create$ ?pos))    ; "rodea" espera una variable multifield
        ;(printout t "Entrando en VERIFICAR en MOV con posm siendo: " $?posm crlf)
        (bind ?valido (verificar ?posm ?c $?mapeo))
        (bind ?est (nth$ ?pos $?mapeo))
        (if (and (and ?valido (eq ?est 0)) (and (<= ?x ?*tamano*) (<= ?y ?*tamano*)))
            then
            (progn
                (printout t "" crlf)
                (printout t "----------------------------------------" crlf)
                (printout t "" crlf)

                (bind $?mapeo (replace$ $?mapeo ?pos ?pos (if (eq ?c b) then b else n)))
                (retract ?tab)
                (assert (tablero (matriz $?mapeo)))

                ; Llamar a comer para verificar y eliminar fichas rodeadas
                ;(printout t "Entrando en comer para verificar y eliminar fichas rodeadas" crlf)
                (bind ?nuevoMapa (comer ?pos ?c $?mapeo))
                ;(printout t "Saliendo de comer. Contenido devuelto: " ?nuevoMapa crlf)
                (if (neq ?nuevoMapa FALSE)
                    then
                    (progn
                        (retract ?tab)
                        (assert (tablero (matriz ?nuevoMapa)))
                        (printout t "Fichas comidas, actualizando tablero." crlf)
                        (imprimir ?nuevoMapa)
                    )
                )
                (bind ?aux FALSE)
            )
        
        else 
            (printout t "Movimiento invalido. Intente de nuevo." crlf)
            (printout t "----------------------------------------" crlf)
            (printout t " " crlf)
        )
    )
    (if (eq ?nuevoMapa FALSE)
            then
            (progn
                (printout t "Tablero después del movimiento del jugador actual: " crlf)
                (imprimir $?mapeo)
            )
    )


    ;; Cambiar la activación del jugador
    (do-for-fact ((?juga jugador)) (eq ?juga:activo FALSE)
        (bind ?ident ?juga)
    )
    (modify ?ident (activo TRUE))
    (modify ?j (activo FALSE))
)


(defrule mov-maquina
    ?j <- (jugador (id ?i) (tipo m) (color ?c) (puntos ?puntos) (activo TRUE))
    ?tab <- (tablero (matriz $?mapeo))
=>
    (printout t "Turno de la máquina..." crlf)
    (bind ?aux FALSE)
    (while (eq ?aux FALSE)
        (bind ?mov (random 1 (* ?*tamano* ?*tamano*)))
        ;(printout t "Posición seleccionada por la máquina: " ?mov crlf)
        (bind $?movm (create$ ?mov))
        ;(printout t "Entrando a VERIFICAR en MOV-MAQUINA con movm siendo: " ?movm crlf)
        (if (and (eq (nth$ ?mov $?mapeo) 0) (verificar ?movm ?c $?mapeo))
            then
            (bind ?aux TRUE)
        )
    )

    (printout t "Posición jugada por la máquina: " ?mov crlf)
    
    (bind $?mapeo (replace$ $?mapeo ?mov ?mov (if (eq ?c b) then b else n)))
    (retract ?tab)
    (assert (tablero (matriz $?mapeo)))

    ; Llamar a comer para verificar y eliminar fichas rodeadas
    ;(printout t "Entrando en comer para verificar y eliminar fichas rodeadas" crlf)
    (bind ?nuevoMapa (comer ?mov ?c $?mapeo))
    ;(printout t "Saliendo de comer. Contenido devuelto: " ?nuevoMapa crlf)
    (if (neq ?nuevoMapa FALSE)
        then
        (progn
            (retract ?tab)
            (assert (tablero (matriz ?nuevoMapa)))
            (imprimir ?nuevoMapa)
        )
    else
        (printout t "Tablero después del movimiento de la máquina: " crlf)
        (imprimir $?mapeo)
    )

    ;; Cambiar la activación del jugador
    (do-for-fact ((?juga jugador)) (eq ?juga:activo FALSE)
        (bind ?ident ?juga)
    )
    (modify ?ident (activo TRUE))
    (modify ?j (activo FALSE))
)