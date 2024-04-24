funcion grupo:
· como parámetro, le voy a pasar la posiciónd ela ultima ficha colocada, ademas de la variable multicampo mapeo

· quiero hacer un recorrido en anchura, para ver cual es la ficha mas cercana de otro color

·una vez encontrada, deberá hacerse otro recorrido en anchura para ver los posibles recorridos que podría hacer el grupo (en caso de que la ficha colocada pueda formar un grupo. si la ficha colocada no está en contacto (8 casillas a su alrededor) o el recorrido no se cierra, no hay grupo). Este recorrido debería seguir el siguiente criterio:
se parte de la ficha contraria al color de la ficha que se ha colocado, y se va extendiendo. hay que comprobar las 4 casillas circundantes a la ficha (dist1). dist 2 tendrá 8 casillas, etc. hay que ver si eso se cierra o no

sistema de puntuación: al final, cambiar a una rule que cuente las fichas. el que tenga mas, gana (no comprobarse)