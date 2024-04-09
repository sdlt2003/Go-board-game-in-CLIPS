
(defglobal ?*tamanoFila* = 0)
(defglobal ?*tamanoColumna* = 0)

(defrule inicio
    (not tablero)
=>  
    
    (printout t "Inserte el tamaño (A) del tablero (AxA). Escoga entre 4, 6 u 9): " crlf)
    (bind ?tamano (read))

    ; inicializacion de tablero
    (bind ?*tamanoFila* ?tamano)
    (bind ?*tamanoColumna* ?tamano)
    (if (eq ?tamano 4) then
        (assert (tablero (id 0) (padre -1) (nivel 0) (matriz (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
    )
    (if (eq ?tamano 6) then
        (assert (tablero (id 0) (padre -1) (nivel 0) (matriz (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
    )
    (if (eq ?tamano 9) then
        (assert (tablero (id 0) (padre -1) (nivel 0) (matriz ( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
    ) 

    (printout t "Inserte tipo del jugador 1 (h/m) : " crlf)
    (bind ?tipo (read))
    (printout t "Inserte color del jugador 1 (b/n) : " crlf)
    (bind ?color (read))

    ; inicializacion de jugador1
    (if (eq ?tipo h) then
        (if (eq ?color b) then
            (assert (jugador (id 1) (tipo h) (color b) (puntos 0)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 1) (tipo h) (color n) (puntos 0)))
        )
    )
    (if (eq ?tipo m) then
        (if (eq ?color b) then
            (assert (jugador (id 1) (tipo m) (color b) (puntos 0)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 1) (tipo m) (color n) (puntos 0)))
        )
    )

    (printout t "Inserte tipo del jugador 2 (h/m) : " crlf)
    (bind ?tipo (read))
    (printout t "Inserte color del jugador 2 (b/n) : " crlf)
    (bind ?color (read))

    ; inicializacion de jugador2
    (if (eq ?tipo h) then
        (if (eq ?color b) then
            (assert (jugador (id 2) (tipo h) (color b) (puntos 0)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 2) (tipo h) (color n) (puntos 0)))
        )
    )
    (if (eq ?tipo m) then
        (if (eq ?color b) then
            (assert (jugador (id 2) (tipo m) (color b) (puntos 0)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 2) (tipo m) (color n) (puntos 0)))
        )
    )

    (assert (turno 1))
)

(defrule mov-jugador1
    (turno 1)
    (jugador (id ))
    (tablero (id ?id) (padre -1) (nivel 0) (matriz ?mapeo))
=>
    ?t <- (turno 1)
    (printout t "Jugador 1, ingresa tu movimiento (x y): ")
    (bind ?x (read))
    (bind ?y (read))
    (assert (movimiento jugador1 ?x ?y))
    
    (retract ?t)
    (assert (turno 2))
)

(defrule mov-jugador2
    (turno 2)
=>
    ?t <- (turno 2)
    (printout t "Jugador 2, ingresa tu movimiento (x y): ")
    (bind ?x (read))
    (bind ?y (read))
    (assert (movimiento jugador2 ?x ?y))
    
    (retract ?t)
    (assert (turno 1))
)

(defrule verificar-movimiento
    (movimiento ?jugador ?x ?y)
    ?pos <- (posicion ?x ?y vacio)
=>
    (retract ?pos)
    (assert (posicion ?x ?y (if (eq ?jugador jugador1) then negro else blanco)))
)


;las siguientes funciones nos las daban en egela. Están modificadas para que se adapten a nuestro juego

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

;Imprimir el estado actual del mapeo
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
			(if (or (eq ?contenido 1)(eq ?contenido -1)) then
                (if (eq ?contenido 1) then
                    (printout t  "  B  |")
                )
				(if (eq ?contenido -1) then
                    (printout t  "  N  |")
                )
			else
				(printout t "     |")
			)
      )
      (generarLineas ?*tamanoFila*)
    )
)