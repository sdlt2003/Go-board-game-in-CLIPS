

(defrule inicio
    (not tablero)
=>  
    (assert (tablero (id 0) (padre -1) (nivel 0) (matriz 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    (assert (jugador (id 1) (tipo h) (color blancas) (fichas 0)))
    (assert (jugador (id 2) (tipo h) (color negras) (fichas 0)))
    (assert (turno 1))
)

(defrule mov-judador1
    (turno 1)
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

(defrule cambiar-turno
    (declare (salience 80))
    ?t <- (turno ?jugador)
=>
    (if (eq ?jugador jugador1)
        then
        (bind ?nuevo-jugador jugador2)
        else
        (bind ?nuevo-jugador jugador1))
    (retract ?t)
    (assert (turno ?nuevo-jugador))
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
(deffunction imprimir ($?tablero)
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
			(if (or (eq ?contenido ficha_blanca)(eq ?contenido ficha_negra)) then
                (if (eq ?contenido ficha_blanca) then
                    (printout t  "  o  |")
                )
				(if (eq ?contenido ficha_negra) then
                    (printout t  "  x  |")
                )
			else
				(printout t "     |")
			)
      )
      (generarLineas ?*tamanoFila*)
    )
)