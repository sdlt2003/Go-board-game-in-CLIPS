
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

; Las siguientes funciones nos las daban en egela. Están modificadas para que se adapten a nuestro juego
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

; esta funcion tiene que comprobar si con la ultima ficha colocada se ha formado un grupo, RETURN array de posiciones
(deffunction grupo($?mapeo )
    ; Obtiene el color de la última ficha colocada
    (bind ?lastColor (nth$ (length$ $?mapeo) $?mapeo))

    ; Inicializa un array para almacenar las posiciones de las fichas en el grupo
    (bind ?groupPositions (create$))

    ; Recorre el tablero de atrás hacia adelante
    (loop-for-count (?i (- (length$ $?mapeo) 1) 1 -1)
        ; Obtiene el color de la ficha actual
        (bind ?currentColor (nth$ ?i $?mapeo))

        ; Si la ficha actual es del mismo color que la última ficha colocada
        (if (eq ?currentColor ?lastColor) then
            ; Añade la posición de la ficha actual al array de posiciones del grupo
            (bind ?groupPositions (insert$ ?groupPositions 1 ?i))
        else
            ; Si la ficha actual no es del mismo color, rompe el bucle
            (break)
        )
    )

    ; Si se ha formado un grupo (al menos una ficha del color opuesto encerrada)
    (if (or (> (length$ ?groupPositions) 3) 
            (and (= (length$ ?groupPositions) 2) (or (= (first$ ?groupPositions) 1) (= (first$ ?groupPositions) (length$ $?mapeo)))))
        ; Devuelve el array de posiciones del grupo
        (return ?groupPositions)
    else
        ; Si no se ha formado un grupo, devuelve un array vacío
        (return (create$))
    )
)

; esta funcion tiene que comprobar si el ultimo movimiento jugado es legal. para ello seguramente
; usará la función grupo
;(deffunction verificar())

; esta funcion tiene que comprobar si con la ultima ficha colocada el jugador come una ficha
; tambien debería sumar puntos al jugador (0 si no come nada, X si come X fichas)
;(deffunction comer())

; esta funcion tiene que evaluar si para el jugador dado por parámetro, le quedan movimientos legales
; para ello, para cada posicion del tablero que este libre, se tiene que ver si se puede colocar una ficha
; en esa posición (con cont > 0 valdría)
;(deffunction evaluar_fin())

; esta funcion tiene que comprobar si con el ultimo movimiento jugado, no existen mas movimientos legales
; para ello, se va a tener que comprobar cada vez que un jugador mueva, si el otro tiene al menos un movimiento
;(deffunction fin())

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
    ?j<-(jugador (id ?i) (tipo h) (color ?c) (puntos ?puntos) (activo TRUE))
    ?tab <- (tablero (matriz $?mapeo))
=>
    (printout t "Tablero: " crlf)
    (imprimir $?mapeo)

    (bind ?aux TRUE)
    (while ?aux do
        (if (eq ?i 1) then
            (printout t "Jugador 1, ingresa tu movimiento (x y):")
        else 
            (printout t "Jugador 2, ingresa tu movimiento (x y):")
        )

        (bind ?x (read))
        (bind ?y (read))
        (bind ?pos (+ (* ?*tamanoFila* (- ?y 1)) ?x))
        (bind ?est (nth$ ?pos $?mapeo))
        
        ; Este modelo no maneja el error de poner 2 blancas o 2 negras seguidas
        ; depende completamente de la buena fe del jugador
        (if (eq ?est 0) then
            (if (eq ?c b) then 
                (retract ?tab)
                (bind $?mapeo (replace$ $?mapeo ?pos ?pos b)) 
                (assert (tablero (matriz $?mapeo))) 
                (imprimir $?mapeo)

            )
            (if (eq ?c n) then
                (retract ?tab)
                (bind $?mapeo (replace$ $?mapeo ?pos ?pos n)) 
                (assert (tablero (matriz $?mapeo))) ; TODO colocar ficha negra
            )
            (bind ?aux FALSE)
        else 
            (printout t "Movimiento invalido" crlf)
        )
    )

    (do-for-fact ((?juga jugador)) (eq ?juga:activo FALSE)
        (bind ?ident ?juga)
    )

    (modify ?ident (activo TRUE))
    (modify ?j (activo FALSE))
)

