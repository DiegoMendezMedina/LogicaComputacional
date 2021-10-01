Integrantes:	
  López Miranda Angel Mauricio.
  Méndez Medina Diego.

Desarrollo de la práctica:
     Nos ayudo lo que vimos en discretas, tanto de haskell como de
    recursión, tambíen uno esta cursando conjuntos y de ahí salio la
    sugerencia de implementar los axiomas de Peano.
     Creamos un repositorio en github para llevar un buen manejo de version,
    uno aprueba las pull request del otro.

     Nos reunimos el ___ a las __ ...
          	 
     Estuvo interesante, decidimos implementar varias cosas extras:
   
      * Suma en la aritmetica de peano, fue extra por que la primera
       solución que se nos ocurrio para la suma tenia que ver con
       potencias de dos y no quisimos quitarla. Ya que teniamos la
       suma de peano decidimos auxiliarnos de esta, como lo hace peano
       para definir el producto en sus axiomas cinco y seis. Con el unico
       cambio que peano utiliza el vació o cero, y acá dado el constructor
       tuvimos que usar el U como caso base. Fue necesario implementar
       la función extra predecesor.
	
      * Eq, para checar que nuestras funciones estuvieran bien.
      
      * Las funciones auxiliares que fueron necesarias:
      
      	- aux_eq, Simplemente checa que el iesimo elemento sea igual para
	ambos binarios, si llegamos a que ambos tienen como ultimo elemento
	U, entonces son el mismo número.
	
	- auxBinANat, hicimos recursión sobre el segundo Binario n veces.
	Si era Uno sumabamos la iesima potencia de dos, si no continuabamos.
	
	- potencia_dos, calculamos la iesima potencia de dos.

	- natListaABin, dada una lista de enteros regresa su representación
	en Binario. Esta función la hicimos usando el hint. Dado el constructor
	de Binario decidimos que la lista debe seguir el mismo patrón, es decir,
	solo se aceptan listas cuyo ultimo elemento es uno y listas con
	unícamente unos y ceros. Función auxiliar para sumaBinLista.
	Ejemplo de entrada valida:    natListaABin [0,0,0,1] = ocho
	Ejemplo de entrada no valida: natListaABin [_,_,_,0] = error
	Ejemplo de entrada no valida: natListaABin [5,0,0,1] = error
	
      	- reversa, por la forma como implementamos la función anterior
	necesitabamos invertir las listas recibidas en sumaBinLista.
      
Dificultades que presentamos:
      Desde que vimos el pdf sabiamos que seria necesario movernos entre
     naturales y Binario. Si bien resultaron más divertidos de implementar
     los meramente Binarios, en algunos decidimos ir por lo más sencillo,
     como lo fue en predecesor.
      Más allá de eso no tuvimos muchas dificultades.
      Aun que no vimos 'guards' ni where en el laboratorio, las conocemos
      del curso de discretas, como tambíen lo fue el Eq, div y mod.
     div siendo la división y mod la operación modulo que suelen considerarse
     operaciones elementales y vienen en la mayoria de los lenguajes
     de programación.


