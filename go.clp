; Definición basica de tablero y jugador

(defglobal ?*tamanoFila* = 0)
(defglobal ?*tamanoColumna* = 0)

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

; Función para determinar si una posición está fuera de los límites del tablero
(deffunction fuera-de-tablero (?pos)
    (bind ?x (mod (- ?pos 1) ?*tamanoFila*))
    (bind ?y (div (- ?pos 1) ?*tamanoFila*))
    (return (or (< ?x 0) (>= ?x ?*tamanoFila*)
                (< ?y 0) (>= ?y ?*tamanoColumna*)))
)


(deffunction obtener-adyacentes (?pos)
    (bind ?x (mod (- ?pos 1) ?*tamanoFila*))
    (bind ?y (div (- ?pos 1) ?*tamanoFila*))
    (bind ?adyacentes (create$))

    (loop-for-count (?dx -1 1) do
        (loop-for-count (?dy -1 1) do
            (if (or (neq ?dx 0) (neq ?dy 0)) 
                then
                (progn
                    (bind ?nx (+ ?x ?dx))
                    (bind ?ny (+ ?y ?dy))
                    (if (and (>= ?nx 0) (< ?nx ?*tamanoFila*)
                             (>= ?ny 0) (< ?ny ?*tamanoColumna*))
                        then
                        (bind ?nPos (+ 1 (+ (* ?ny ?*tamanoFila*) ?nx)))
                        (if (> (length$ ?adyacentes) 0)
                            then (bind ?adyacentes (insert$ ?adyacentes (length$ ?adyacentes) ?nPos))
                            else (bind ?adyacentes (create$ ?nPos))
                        )
                    )
                )
            )
        )
    )
    (return ?adyacentes)
)


(deffunction adyacente-a-oponente (?pos ?ultimoColor $?mapeo)

    (bind ?contenido (nth$ ?pos $?mapeo))
    (return (neq ?contenido ?ultimoColor))
)

; //////////////////////////////

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
    (loop-for-count (?i 0 ?*tamanoFila*) do
      (if (= ?i 0) then
      (printout t "       ")
      else
      (printout t "  "?i "   "))
    )
    (generarLineas ?*tamanoFila*)
    (loop-for-count (?fila 1 ?*tamanoFila* ) do
      (generarLineas2 ?*tamanoFila*)
      (printout t "   " ?fila "  |" )
      (loop-for-count (?columna 1 ?*tamanoFila*) do
            (bind ?contenido (nth$  (+ (* ?*tamanoFila* (- ?fila 1)) ?columna) $?mapeo))
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
      (generarLineas ?*tamanoFila*)
    )
)
; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



(deffunction grupo_uwu (?pos $?mapeo)
    (bind ?ultimoColor (nth$ (- (length$ $?mapeo) 1) $?mapeo))
    (bind ?gruposRodeados (create$))
    (bind ?cola (create$ ?pos))

    (printout t "haciendo uwu para uwu: " ?pos " Color: " ?ultimoColor crlf)

    (while (neq (length$ ?cola) 0)
        (bind ?actual (nth$ 1 ?cola))
        (bind ?cola (rest$ ?cola))
        (bind ?posicionesAdyacentes (obtener-adyacentes ?actual))

        (foreach ?pos ?posicionesAdyacentes
            (if (and (not (fuera-de-tablero ?pos)) (adyacente-a-oponente ?pos ?ultimoColor $?mapeo))
                then
                (progn
                    (printout t "grupo rodeado: " (implode$ ?gruposRodeados) crlf)
                    (if (not (member$ ?pos ?gruposRodeados))
                        then
                        (bind ?gruposRodeados (insert$ ?gruposRodeados 1 ?pos))
                    )
                )
            )
        )
    )

    (if (> (length$ ?gruposRodeados) 0)
        then (return ?gruposRodeados)
        else (return FALSE)
    )
)




(deffunction comer (?pos $?mapeo)
    (bind ?resultados (grupo_uwu ?pos $?mapeo))
    (if (neq ?resultados FALSE)
        then
        (progn
            (printout t "Fichas a eliminar en las posiciones: " (implode$ ?resultados) crlf)
            (foreach ?p ?resultados
                (bind $?mapeo (replace$ $?mapeo ?p ?p 0))
            )
            (return $?mapeo)
        )
        else
        (return FALSE)
    )
)






; ///////////////////////////////////////////////////

; esta funcion tiene que comprobar si el ultimo movimiento jugado es legal. para ello seguramente
; usará la función grupo
;(deffunction verificar())

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
    (bind ?tamano (read))

    ; inicializacion de tablero
    (bind ?*tamanoFila* ?tamano)
    (bind ?*tamanoColumna* ?tamano)
    (if (eq ?tamano 4) then
        (assert (tablero (id 0) (padre -1) (nivel 0) (matriz 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    )
    (if (eq ?tamano 6) then
        (assert (tablero (id 0) (padre -1) (nivel 0) (matriz 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    )
    (if (eq ?tamano 9) then
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
    (printout t "Tablero: " crlf)
    (imprimir $?mapeo)

    (bind ?aux TRUE)
    (while ?aux do
        (printout t (if (eq ?i 1) then "Jugador 1, " else "Jugador 2, ") "ingresa tu movimiento (x y):")
        (bind ?x (read))
        (bind ?y (read))
        (bind ?pos (+ (* ?*tamanoFila* (- ?y 1)) ?x))

        (bind ?est (nth$ ?pos $?mapeo))
        (if (eq ?est 0)
            then
            (progn
                (bind $?mapeo (replace$ $?mapeo ?pos ?pos (if (eq ?c b) then b else n)))
                (retract ?tab)
                (assert (tablero (matriz $?mapeo)))
                (imprimir $?mapeo)

                ; Llamar a comer para verificar y eliminar fichas rodeadas
                (bind ?nuevoMapa (comer ?pos $?mapeo))
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
        )
    )

    ;; Cambiar la activación del jugador
    (do-for-fact ((?juga jugador)) (eq ?juga:activo FALSE)
        (bind ?ident ?juga)
    )
    (modify ?ident (activo TRUE))
    (modify ?j (activo FALSE))
)
