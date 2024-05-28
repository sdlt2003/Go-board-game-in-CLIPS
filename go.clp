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


; Función para determinar si una posición está fuera de los límites del tablero
(deffunction fuera-de-tablero (?pos, ?x, ?y)
    (if (or (<= ?x 0) (<= ?y 0) (>= ?x ?*tamanoFila*) (>= ?y ?*tamanoColumna*))
        then (return TRUE)
        else (return FALSE)
    )
)

(deffunction pos-a-coord (?pos)
    (bind ?x (mod ?pos ?*tamanoFila*))
    (bind ?y (+ 1 (div ?pos ?*tamanoFila*)))
    (return (create$ ?x ?y))
)

(deffunction coord-a-pos (?x ?y)
    (return (+ 1 (+ (* ?y ?*tamanoFila*) ?x)))
)


(deffunction adyacente-a-oponente (?pos ?ultimoColor $?mapeo)
    (bind ?contenido (nth$ ?pos $?mapeo))
    (return (neq ?contenido ?ultimoColor))
)


(deffunction obtener-adyacentes (?pos ?color1 $?mapeo)

    (bind ?adyacentes (create$))                ; inicializamos lista
    (bind ?x (nth$ (pos-a-coord ?pos) 1))       ; obtenemos la coordenada x
    (bind ?y (nth$ (pos-a-coord ?pos) 2))       ; obtenemos la coordenada y
    (loop-for-count (?dx -1 1) do               ; creamos coordenadas locales para poder recorrer los adyacentes
        (loop-for-count (?dy -1 1) do           ; de forma sencilla
            (if (or (neq ?dx 0) (neq ?dy 0))    ; excluimos la posición actual 
                then
                (progn
                    (bind ?nx (+ ?x ?dx))       ; calculamos la posicion real
                    (bind ?ny (+ ?y ?dy))    
                    (if (not (fuera-de-tablero ?nx ?ny)) then   ; si no está fuera del tablero
                        (bind ?nPos (coord-a-pos ?nx ?ny))
                        (bind ?est (nth$ ?nPos $?mapeo))
                        (if (eq ?est ?color1) then              ; si es del color del jugador
                            (if (> (length$ ?adyacentes) 0) then
                                then (bind ?adyacentes (insert$ ?adyacentes (length$ ?adyacentes) ?nPos))
                                else (bind ?adyacentes (create$ ?nPos))
                            )
                        )
                    )
                )
            )
        )
    )
    (return ?adyacentes)
)



(deffunction grupo(?pos ?c ?$?mapeo)
    ; Obtiene el color de la última ficha colocada
    ; Con esto, podemos asignar a color1: x; color2: y; asi no repetimos code en ningun lado
    (if (eq ?c b) then
        (bind ?color1 b)
        (bind ?color2 n)
    )
    (if (eq ?c n) then
        (bind ?color1 n)
        (bind ?color2 b)
    )

    ; Inicializa un array para almacenar los grupos que se cierran:
    ; IMPORTANTE no existen multicampos de multicampos. Almacenar como un multicampo de strings
    ; despues, coger mediante nth$ y tratar cada string como un multicampo en si mismo
    (bind ?gruposRodeados (create$))    ; se agrega con: (bind ?g_c (insert$ ?g_c (+ (length$ ?g_c) 1) "nuevoString"))
    (bind ?cola (create$ ?pos))         ; Inicializa una cola para el recorrido en anchura
    (bind ?inicial ?pos)                ; Guarda la posición inicial para comprobar si se ha cerrado el grupo

    (while (neq (length$ ?cola) 0)      ; Mientras haya elementos en la cola
        (bind ?actual (nth$ 1 ?cola))   ; Obtiene la primera posición de la cola
        (bind ?cola (rest$ ?cola))      ; Elimina la primera posición de la cola
        (bind ?posicionesAdyacentes (obtener-adyacentes ?actual))   ;  Obtiene las posiciones adyacentes a la actual

        (foreach ?pos ?posicionesAdyacentes  ; Por cada posición adyacente
            (bind ?x (nth$ (pos-a-coord ?pos) 1))   ; Obtiene la coordenada x
            (bind ?y (nth$ (pos-a-coord ?pos) 2))   ; Obtiene la coordenada y
            (if (not (fuera-de-tablero ?pos ?x ?y))
                then
                (progn
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
    (bind ?res (grupo ?pos $?mapeo))
    (if (neq ?res FALSE) ; IF grupo devuelve un grupo de fichas rodeadas
        then
        (progn
            (foreach ?p ?res
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
