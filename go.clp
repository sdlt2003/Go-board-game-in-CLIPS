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
    (slot pass)
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

(deffunction ejes (?pos)
    (bind ?coords (pos-a-coord ?pos))
    (bind ?x (nth$ 1 ?coords))
    (bind ?y (nth$ 2 ?coords))
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
    (return (create$ ?norte ?sur ?este ?oeste))
)

(deffunction encruzijada (?pos ?c1 $?mapeo)
    (bind ?ejes (ejes ?pos))
    (printout t "Ejes (dentro de encruzijada): " ?ejes crlf)
    (bind ?enc TRUE)
    (foreach ?eje ?ejes
        (if (neq ?eje f) 
        then
            progn
            (if (or (eq (nth$ ?eje ?mapeo) 0) (eq (nth$ ?eje ?mapeo) ?c1))
            then
                (bind ?enc FALSE)
            )
        )
    )
    (return ?enc)
)


(deffunction suelta (?pos ?c1 $?mapeo)
    (bind ?ejes (ejes ?pos))
    (bind ?suelta TRUE)
    (foreach ?eje ?ejes
        (if (neq ?eje 0)
            then
            (bind ?suelta FALSE)
        )
    )
    return ?suelta
)


; grupo := grupo de fichas adyacentes a la posición ?pos que se pueden comer
; (se trata como una variable multicampo porque )
; c     := color de las fichas que comen
(deffunction rodea (?grupo ?cEnemigas $?mapeo)
    (if (eq ?cEnemigas b) then
        (bind ?cAliadas n)
    else
        (bind ?cAliadas b)
    )

    (bind $?acomer (create$))

    (foreach ?pos ?grupo
        
        (bind ?enc (encruzijada ?pos ?cAliadas $?mapeo))
        (printout t "Resultado de ENCRUZIJADA en RODEA: " ?enc crlf)
        
        ; caso básico 1: ficha unica rodeada
        (if ?enc then
            (bind $?acomer (create$ $?acomer ?pos))
        else
            (printout t "Comprobando el caso general dentro de RODEA..." crlf)
            (bind ?visitados (create$ ?pos))
            (bind ?cola (create$ ?pos))
            (bind ?escape FALSE)
            
            (while (and (> (length$ ?cola) 0) (eq ?escape FALSE)) do
                (bind ?actual (nth$ 1 ?cola))
                (bind ?ejes (ejes ?actual))

                (printout t "Ejes de la ficha actual: " ?ejes crlf)

                (bind ?visitados (create$ ?visitados ?actual))
                (bind ?cola (delete-member$ ?cola ?actual))
                (bind ?i (nth$ 1 ?ejes))
                
                (while (> (length$ ?ejes) 0) do
                    (if (eq ?i f) then
                            (printout t "Eje " ?i " es f" crlf)
                    else
                        (if (eq (nth$ ?i ?mapeo) 0)
                            then
                            (bind ?escape TRUE)
                        else
                            (if (and (not (member$ ?i ?visitados)) (eq (nth$ ?i ?mapeo) ?cAliadas))
                                then
                                (bind ?visitados (create$ ?visitados ?i))
                                (bind ?cola (create$ ?cola ?i))
                            )
                        )
                    )
                    (bind ?ejes (delete-member$ ?ejes ?i))
                    (bind ?i (nth$ 1 ?ejes))
                )          
                (bind ?cola (delete-member$ ?cola ?actual))      
            )
            (if (eq ?escape FALSE)
                then
                (bind $?acomer (create$ $?acomer ?visitados))
            else
                (printout t "No se puede comer nada" crlf)
                (return FALSE)
            )
        )
    )
    (printout t "Resultado de RODEA (fichas que se van a comer): " $?acomer crlf)
    (printout t "" crlf)
    (return $?acomer)
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

    (printout t "Obteniendo fichas enemigas adyacentes a la ficha colocada..." crlf)
    (bind ?grupoEnemigo (obtener-adyacentes ?pos ?color2 $?mapeo))
    (printout t "Adyacentes: " ?grupoEnemigo crlf)

    (if (neq (length$ ?grupoEnemigo) 0)
        then
        (progn
            ; Verifica si las fichas capturables están rodeadas por el grupo
            (printout t "Entrando en RODEA para verificar si se come alguna ficha..." crlf)
            (printout t "" crlf)

            (bind ?res (rodea ?grupoEnemigo ?color1 $?mapeo))
            (if (neq ?res FALSE)
                then 
                    (printout t "Se debe(n) comer ficha(s)" crlf)
                    (return ?res)
                else 
                    (printout t "No se debe comer nada" crlf)
                    (return FALSE)
            )
        )
    )
    (return FALSE)
)



(deffunction comer (?pos ?c $?mapeo)
    (printout t "Entrando en grupo para ver si tocamos alguna ficha adyacente enemiga" crlf)
    (printout t "" crlf)

    (bind ?res (grupo ?pos ?c $?mapeo)) ; tengo que conseguir que res sea una lista de posiciones de fichas a eliminar
    (if (neq ?res FALSE) ; IF no es FALSE, entonces 
        then
        (progn
            (printout t "Se van a eliminar las fichas de las siguientes posiciones: " ?res crlf)
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

(deffunction ganador (?mapeo)
    (bind ?puntosB 0)
    (bind ?puntosN 0)
    (loop-for-count (?i 1 (* ?*tamano* ?*tamano*)) do
        (bind ?contenido (nth$ ?i ?mapeo))
        (if (eq ?contenido b) then
            (bind ?puntosB (+ ?puntosB 1))
        )
        (if (eq ?contenido n) then
            (bind ?puntosN (+ ?puntosN 1))
        )
    )
    (if (> ?puntosB ?puntosN) then
        (return "las blancas")
    )
    (if (> ?puntosN ?puntosB) then
        (return "las negras")
    )
    (return "oh! empate")
)

; ///////////////////////////////////////////////////


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
            (assert (jugador (id 2) (tipo h) (color b) (puntos 0) (activo FALSE) (pass FALSE)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 2) (tipo h) (color n) (puntos 0) (activo FALSE) (pass FALSE)))
        )
    )
    (if (eq ?tipo m) then
        (if (eq ?color b) then
            (assert (jugador (id 2) (tipo m) (color b) (puntos 0) (activo FALSE) (pass FALSE)))
        )
        (if (eq ?color n) then
            (assert (jugador (id 2) (tipo m) (color n) (puntos 0) (activo FALSE) (pass FALSE)))
        )
    )
)


(defrule mov
    ?j <- (jugador (id ?i) (tipo h) (color ?c) (puntos ?puntos) (activo TRUE) (pass ?pass))
    ?tab <- (tablero (matriz $?mapeo))
=>
    (bind ?movimientoValido FALSE)
    (while (eq ?movimientoValido FALSE) do
        (printout t (if (eq ?i 1) then "Jugador 1, " else "Jugador 2, ") "ingresa tu movimiento (x y) o acaba la partida (p p):")
        (bind ?x (read))
        (bind ?y (read))

        (if (and (eq ?x p) (eq ?y p))
            then
                (printout t "Acabando la partida..." crlf)
                (modify ?j (pass TRUE))
                (return)
        )

        (bind ?pos (+ (* ?*tamano* (- ?y 1)) ?x))
        (printout t "Posicion jugada: " ?pos crlf)

        (printout t "Verificando si el movimiento es válido..." crlf)
        (bind ?enc (encruzijada ?pos ?c $?mapeo))
        (printout t "Resultado de ENCRUZIJADA: " ?enc crlf)
        (bind ?est (nth$ ?pos $?mapeo))

        (if (and (and (eq ?enc FALSE) (eq ?est 0)) (and (<= ?x ?*tamano*) (<= ?y ?*tamano*)))
            then
                (printout t "Movimiento valido." crlf)
                (bind $?mapeo (replace$ $?mapeo ?pos ?pos (if (eq ?c b) then b else n)))
                (retract ?tab)
                (assert (tablero (matriz $?mapeo)))

                ; Llamar a comer para verificar y eliminar fichas rodeadas
                (printout t "Entrando en comer para verificar y eliminar fichas rodeadas" crlf)
                (bind ?nuevoMapa (comer ?pos ?c $?mapeo))
                
                (if (neq ?nuevoMapa FALSE)
                    then
                        (retract ?tab)
                        (assert (tablero (matriz ?nuevoMapa)))
                        (printout t "Fichas comidas, actualizando tablero." crlf)
                )
                (bind ?movimientoValido TRUE)
            else
                (printout t "Movimiento invalido. Intente de nuevo." crlf)
                (printout t "----------------------------------------" crlf)
                (printout t " " crlf)
        )
    )

    (printout t "Tablero después del movimiento del jugador actual: " crlf)
    (if (neq ?nuevoMapa FALSE) then
        (imprimir ?nuevoMapa)
    else
        (imprimir $?mapeo)
    )
    (printout t "----------------------------------------" crlf)

    ;; Cambiar la activación del jugador
    (do-for-fact ((?juga jugador)) (eq ?juga:activo FALSE)
        (modify ?juga (activo TRUE))
    )
    (modify ?j (activo FALSE))

    (printout t "Turno completado." crlf)
)


(defrule mov-maquina
    ?j <- (jugador (id ?i) (tipo m) (color ?c) (puntos ?puntos) (activo TRUE) (pass ?pass))
    ?tab <- (tablero (matriz $?mapeo))
=>
    (printout t "Turno de la máquina..." crlf)
    (bind ?aux FALSE)
    (bind ?lol 0)
    (while (eq ?aux FALSE)
        (bind ?mov (random 1 (* ?*tamano* ?*tamano*)))
        (printout t "Posición seleccionada por la máquina: " ?mov crlf)
        (if (and (eq (nth$ ?mov $?mapeo) 0) (eq (encruzijada ?mov ?c $?mapeo) FALSE))
            then
            (bind ?aux TRUE)
        )
        (bind ?lol (+ ?lol 1))
        (if (eq ?lol 200)
            then
            (printout t "La máquina no puede realizar un movimiento válido. Pasando el turno..." crlf)
            (modify ?j (pass TRUE))
            break
        )
    )

    (if (eq ?aux TRUE) then
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
)
    

(defrule fin
    (declare (salience 10))
    ?tab <- (tablero (matriz $?mapeo))
    ?j <- (jugador (id ?i) (tipo ?t) (color ?c) (puntos ?puntos) (activo ?act) (pass TRUE))
=>
    (printout t "" crlf)
    (printout t "------------------------------------------" crlf)
    (printout t "" crlf)

    (printout t "Juego terminado." crlf)

    (printout t "" crlf)
    (printout t "------------------------------------------" crlf)
    (printout t "" crlf)

    (bind ?p1 (ganador ?mapeo))
    (printout t "Ganador: " ?p1 "!" crlf)
    
    (halt)
)