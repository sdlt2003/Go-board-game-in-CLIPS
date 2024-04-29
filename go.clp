
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
    (bind $?grupos_cerrados (create$))  ; se agrega con: (bind ?g_c (insert$ ?g_c (+ (length$ ?g_c) 1) "nuevoString"))
    (bind $?cola (create$ ?pos)) ; Inicializa una cola para el recorrido en anchura
    (bind ?inicial ?pos) ; Guarda la posición inicial para comprobar si se ha cerrado el grupo

    (while (> (length$ ?queue) 0); WHILE la cola no esté vacía

        (bind ?pos_actual (nth$ 1 ?cola)) ; Obtiene la primera posición de la cola
        (bind ?x (mod ?pos_actual ?*tamanoFila*)) ; Obtiene la columna de la posición actual
        (bind ?y (+ (/ ?pos_actual ?*tamanoFila*) 1)) ; Obtiene la fila de la posición actual

        (bind ?cola (delete$ ?cola 1 1)) ; Elimina la primera posición de la cola

        ; Se obtienen las posiciones adyacentes a la posición actual
        (bind $?adyacentes (create$))
        (bind ?i 1)
        (while (< i 8)
            (switch ?i
                case 1
                    (if (< ?x ?*tamanoFila*) ; IF la posición no está en el borde derecho
                        (bind $?adyacentes (insert$ $?adyacentes (length$ $?adyacentes) (+ ?pos_actual 1))) ; Obtiene la posición de la derecha
                    )
                case 2
                    (if (> ?x 1) ; IF la posición no está en el borde izquierdo
                        (bind $?adyacentes (insert$ $?adyacentes (length$ $?adyacentes) (- ?pos_actual 1))) ; Obtiene la posición de la izquierda
                    )
                case 3
                    (if (< ?y ?*tamanoColumna*) ; IF la posición no está en el borde inferior
                        (bind $?adyacentes (insert$ $?adyacentes (length$ $?adyacentes) (+ ?pos_actual ?*tamanoFila*))) ; Obtiene la posición de abajo
                    )
                case 4
                    (if (> ?y 1) ; IF la posición no está en el borde superior
                        (bind $?adyacentes (insert$ $?adyacentes (length$ $?adyacentes) (- ?pos_actual ?*tamanoFila*))) ; Obtiene la posición de arriba
                    )
                case 5
                    (if (and (< ?x ?*tamanoFila*) (< ?y ?*tamanoColumna*)) ; IF la posición no está en el borde derecho ni inferior
                        (bind $?adyacentes (insert$ $?adyacentes (length$ $?adyacentes) (+ ?pos_actual (+ ?*tamanoFila* 1)))) ; Obtiene la posición de la diagonal inferior derecha
                    )
                case 6
                    (if (and (> ?x 1) (< ?y ?*tamanoColumna*)) ; IF la posición no está en el borde izquierdo ni inferior
                        (bind $?adyacentes (insert$ $?adyacentes (length$ $?adyacentes) (- ?pos_actual (+ ?*tamanoFila* 1)))) ; Obtiene la posición de la diagonal inferior izquierda
                    )
                case 7
                    (if (and (< ?x ?*tamanoFila*) (> ?y 1)) ; IF la posición no está en el borde derecho ni superior
                        (bind $?adyacentes (insert$ $?adyacentes (length$ $?adyacentes) (+ ?pos_actual (- ?*tamanoFila* 1)))) ; Obtiene la posición de la diagonal superior derecha
                    )
                case 8
                    (if (and (> ?x 1) (> ?y 1)) ; IF la posición no está en el borde izquierdo ni superior
                        (bind $?adyacentes (insert$ $?adyacentes (length$ $?adyacentes) (- ?pos_actual (- ?*tamanoFila* 1)))) ; Obtiene la posición de la diagonal superior izquierda
                    )      
            )
            (bind ?i (+ ?i 1))
        )

        ; Se comprueban las 8 posiciones adyacentes a la posición actual:
        ; IMPORTANTE:   debe haber una variable que mantenga la posicion actual como "anterior"
        ;               para no entrar en bucles infinitos, etc.
        (bind ?i 1)
        (while (< ?i length$ $?adyacentes)
            
            (bind ?pos_actual (nth$ ?i $?adyacentes))

            ; Comprobar que no vas a comprobar la posicion de la que vienes
            (if (eq ?pos_actual ?anterior) ; IF la posición adyacente es la anterior
                (bind ?i (+ ?i 1)) ; Se salta la comprobación
            )


            (bind ?est (nth$ ?pos_actual $?mapeo))
            (if (eq ?est ?color1) ; IF la posición adyacente es del mismo color
                (bind $?cola (insert$ ?cola (length$ ?cola) ?pos_actual)) ; Se añade a la cola
            )
            (bind ?i (+ ?i 1))
        )
        ; De entre todas, coger solo la que esté mas cerca de una ficha del otro color;
        ; (esto probablemente requiera otro recorrido en anchura)

        ; IF la ficha agregada es la inicial
            ; la lista se ordena (a decidir como) para su futura comprobación
            ; IF la lista no está en la lista de grupos cerrados
                ; Se añade a la lista de grupos cerrados

    ; END WHILE
        
    ; IF se ha formado un grupo (al menos una ficha del color opuesto encerrada)

        ; Devuelve TRUE

        ; Si no se ha formado un grupo, devuelve FALSE    
    )
    (return FALSE)

) ; END FUNCTION


(deffunction comer(?pos ?c $?mapeo)

    (if (grupo(?pos ?c $?mapeo)); llamada a grupo ; IF grupo = TRUE
        (bind $?mapeo eliminar(?pos $?mapeo)) ; Se eliminan las blancas que estén dentro del grupo
    )
    
    (return $?mapeo) ; Devuelve el tablero actualizado

) ; END FUNCTION

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

            )
            (if (eq ?c n) then
                (retract ?tab)
                (bind $?mapeo (replace$ $?mapeo ?pos ?pos n)) 
            )
            
            ; (bind $?mapeo (comer ?pos ?c $?mapeo))
            (assert (tablero (matriz $?mapeo)))
            (imprimir $?mapeo)

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

