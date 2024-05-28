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

(deffunction obtener-adyacentes (?fila ?columna ?matriz)
    (bind ?adyacentes (create$))
    ; norte
    (if (and (> ?fila 1) (neq (nth$ (+ (* ?*tamanoFila* (- ?fila 2)) ?columna) ?matriz) nil)) then
        (bind ?adyacentes (create$ (create$ (- ?fila 1) ?columna) $?adyacentes)))
    ; sur
    (if (and (< ?fila ?*tamanoFila*) (neq (nth$ (+ (* ?*tamanoFila* ?fila) ?columna) ?matriz) nil)) then
        (bind ?adyacentes (create$ (create$ (+ ?fila 1) ?columna) $?adyacentes)))
    ; este
    (if (and (> ?columna 1) (neq (nth$ (+ (* ?*tamanoFila* (- ?fila 1)) (- ?columna 1)) ?matriz) nil)) then
        (bind ?adyacentes (create$ (create$ ?fila (- ?columna 1)) $?adyacentes)))
    ; oeste
    (if (and (< ?columna ?*tamanoColumna*) (neq (nth$ (+ (* ?*tamanoFila* (- ?fila 1)) (+ ?columna 1)) ?matriz) nil)) then
        (bind ?adyacentes (create$ (create$ ?fila (+ ?columna 1)) $?adyacentes)))
    ?adyacentes
)


(deffunction grupo (?fila ?columna ?color ?matriz ?visitados)
    (printout t "Entrando en grupo con ?fila: " ?fila " ?columna: " ?columna crlf)
    (bind ?adyacentes (obtener-adyacentes ?fila ?columna ?matriz))
    (printout t "Adyacentes: " ?adyacentes crlf)
    (bind ?grupo (create$ (create$ ?fila ?columna)))
    (bind ?visitados (create$ (create$ ?fila ?columna) $?visitados))
    (foreach ?ady ?adyacentes
        (bind ?coord ?ady)
        (printout t "Procesando adyacente: " ?coord crlf)
        (printout t "nth$ 1 de ?coord: " (nth$ 1 ?coord) crlf)
        (printout t "nth$ 2 de ?coord: " (nth$ 2 ?coord) crlf)
        (if (and (eq (nth$ (+ (* ?*tamanoFila* (- (nth$ 1 ?coord) 1)) (nth$ 2 ?coord)) ?matriz) ?color)
                 (not (member$ ?coord ?visitados)))
            then
            (printout t "Llamando recursivamente a grupo con ?coord: " ?coord crlf)
            (bind ?resultado (grupo (nth$ 1 ?coord) (nth$ 2 ?coord) ?color ?matriz ?visitados))
            (bind ?subgrupo (nth$ 1 ?resultado))
            (bind ?subvisitados (nth$ 2 ?resultado))
            (bind ?grupo (union$ ?grupo ?subgrupo))
            (bind ?visitados (union$ ?visitados ?subvisitados))
        )
    )
    (create$ ?grupo ?visitados)
)

(deffunction esta-rodeado (?grupo ?color ?matriz)
    (bind ?rodeado TRUE)
    (bind ?oponente (if (eq ?color b) then n else b))
    (foreach ?coord ?grupo
        (bind ?adyacentes (obtener-adyacentes (nth$ 0 ?coord) (nth$ 1 ?coord) ?matriz))
        (foreach ?ady ?adyacentes
            (bind ?pos (nth$ (+ (* ?*tamanoFila* (- (nth$ 0 ?ady) 1)) (nth$ 1 ?ady)) ?matriz))
            (if (and (neq ?pos ?color) (neq ?pos ?oponente)) then
                (bind ?rodeado FALSE)
            )
        )
    )
    ?rodeado
)

(deffunction eliminar-grupo (?grupo ?matriz)
    (foreach ?coord ?grupo
        (bind ?indice (+ (* ?*tamanoFila* (- (nth$ 0 ?coord) 1)) (nth$ 1 ?coord)))
        (bind ?matriz (replace$ ?matriz ?indice ?indice 0))
    )
    ?matriz
)

(deffunction comer (?fila ?columna ?color ?matriz)
    (printout t "Entrando en comer con ?fila: " ?fila " ?columna: " ?columna " ?color: " ?color crlf)
    (bind ?grupo-info (grupo ?fila ?columna ?color ?matriz (create$)))
    (printout t "?grupo-info: " ?grupo-info crlf)
    (bind ?grupo (nth$ 1 ?grupo-info))
    (printout t "Grupo obtenido: " ?grupo crlf)
    (if (esta-rodeado ?grupo ?color ?matriz) then
        (printout t "Grupo está rodeado. Eliminando grupo." crlf)
        (bind ?matriz (eliminar-grupo ?grupo ?matriz))
    )
    ?matriz
)

(defrule actualizar-tablero
    ?jugador-n <- (jugador (color n) (activo TRUE))
    ?jugador-b <- (jugador (color b) (activo TRUE))
    ?tablero <- (tablero (matriz $?matriz))
    =>
    (bind ?nueva-matriz $?matriz)
    (loop-for-count (?fila 1 ?*tamanoFila*)
        (loop-for-count (?columna 1 ?*tamanoColumna*)
            (bind ?pos (nth$ (+ (* ?*tamanoFila* (- ?fila 1)) ?columna) ?nueva-matriz))
            (if (neq ?pos 0)
                then
                (bind ?nueva-matriz (comer ?fila ?columna ?pos ?nueva-matriz))
            )
        )
    )
    (modify ?tablero (matriz ?nueva-matriz))
    (imprimir ?nueva-matriz)
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

                (bind ?nuevoMapa (comer ?y ?x ?c $?mapeo))
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