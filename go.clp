
(deffacts tablero-inicial
    (posicion 1 1 estado vacio) (posicion 2 1 estado vacio) (posicion 3 1 estado vacio) (posicion 4 1 estado vacio) (posicion 5 1 estado vacio) (posicion 6 1 estado vacio)
    (posicion 3 1 estado vacio) (posicion 3 2 estado vacio) (posicion 3 3 estado vacio) (posicion 3 4 estado vacio) (posicion 3 5 estado vacio) (posicion 3 6 estado vacio)
    (posicion 4 1 estado vacio) (posicion 4 2 estado vacio) (posicion 4 3 estado vacio) (posicion 4 4 estado vacio) (posicion 4 5 estado vacio) (posicion 4 6 estado vacio)
    (posicion 5 1 estado vacio) (posicion 5 2 estado vacio) (posicion 5 3 estado vacio) (posicion 5 4 estado vacio) (posicion 5 5 estado vacio) (posicion 5 6 estado vacio)
    (posicion 6 1 estado vacio) (posicion 6 2 estado vacio) (posicion 6 3 estado vacio) (posicion 6 4 estado vacio) (posicion 6 5 estado vacio) (posicion 6 6 estado vacio)
    (imprimir)
    
)

(deffacts inicio-juego
    (turno jugador1)
)

(defrule pedir-movimiento-jugador1
    (declare (salience 90)) 
    (turno 1)
=>
    (printout t "Jugador 1, ingresa tu movimiento (x y): ")
    (bind ?x (read))
    (bind ?y (read))
    (assert (movimiento jugador1 ?x ?y))
)

(defrule pedir-movimiento-jugador2
    (declare (salience 90))
    (turno 2)
    (jugador (id 2) (color blancas)....)
=>
    (printout t "Jugador 2, ingresa tu movimiento (x y): ")
    (bind ?x (read))
    (bind ?y (read))
    (assert (movimiento jugador2 ?x ?y))
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


(deffunction get-posicion-estado (?x ?y)
    (bind ?estado vacio) 
    (do-for-fact ((?p posicion)) (and (eq ?p:x ?x) (eq ?p:y ?y))
        (bind ?estado (fact-slot-value ?p estado))
    )
    (return ?estado)
)


(defrule imprimir-tablero
    ?i<-(imprimir)
    (turno ?x)
=>
    (printout t "  1 2 3 4 5 6 7 8 9" crlf)
    (loop-for-count (?y 1 9)
        (printout t ?y " ")
        (loop-for-count (?x 1 9)
            (bind ?estado (get-posicion-estado ?x ?y))
            (if (eq ?estado negro) then
                (printout t "X ")
            else
                (if (eq ?estado blanco) then
                    (printout t "O ")
                else
                    (printout t ". "))
            )
        )
        (printout t crlf)   
    )
    (retract ?i)
    (if (eq ?x 1) then)
)

