

; Revisa una lista de argumentos evaluados.
; Recibe la lista y, si esta contiene alguna sublista que comienza con '*error*, retorna esa sublista. Si no, retorna nil.
(defn revisar-lae [lis] (
   if
   (empty? lis)
   nil
   (
       if
        (seq? (first lis))
        (
            if
            (= (first (first lis))'*error*)
            (first lis)
            (revisar-lae (drop 1 lis))
        )
        (revisar-lae (drop 1 lis))
    )
))




; Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas con comillas) y devuelve su valor. Muestra errores sin parentesis.
; Aridad 1: Si recibe un escalar, lo imprime con salto de linea en formato estandar (pero si es \space no lo imprime), purga la salida y devuelve el escalar.
; Si recibe una secuencia cuyo primer elemento es '*error*, se llama recursivamente con dos argumentos iguales: la secuencia recibida.
; Si no, imprime lo recibido con salto de linea en formato estandar, purga la salida y devuelve la cadena.
; Aridad 2: Si el primer parametro es nil, imprime un salto de linea, purga la salida y devuelve el segundo parametro.
; Si no, imprime su primer elemento en formato estandar, imprime un espacio y se llama recursivamente con la cola del primer parametro y el segundo intacto.
(defn imprimir
   ([elem] (
	   cond
       (= elem \space) (do (flush) '\space)
       (seq? elem) (
                        if
                        (= (first elem) '*error*)
                        (imprimir elem elem)     
                        (do (prn elem) (flush) elem)
                    )
       true(
            if
            (string? elem) 
            (do (prn elem) (str elem))
            (do (prn elem) elem)
       )
       )
    )
    ([lis ori] (  
        if
        (= lis nil)
        (do (prn) (flush) ori) 
        (do (pr (first lis)) (print " ") (imprimir (drop 1 lis) ori)) 
        )
    )
)

;actv2
(defn actualizar-amb [amb-global clave valor] 
  ( 
   if
   (and (seq? valor) (= '*error* (first valor)))
   amb-global
   (if
    (contains? (set amb-global) clave)
    ((fn ciclo [a b] (
        if
        (= (first a) clave)
        (reverse (into (list) (assoc (into [] amb-global) (inc b) (+ valor (nth a 1)))))
        (ciclo (drop 1 a) (inc b))
    )
    ) amb-global 0)
    (concat amb-global (list clave valor))
   )
  )
)
; Verifica la igualdad de dos simbolos.
; Recibe dos simbolos a y b. Retorna true si se deben considerar iguales; si no, false.
; Se utiliza porque TLC-LISP no es case-sensitive y ademas no distingue entre nil y la lista vacia.
(defn igual? [a b] (
        cond 
             (= a b) true
             (and (= a nil ) (not (= nil b)) (= (subs (str '(nil)) 1 4) (.toLowerCase (str b)))) true
             (and (= b nil ) (not (= nil a)) (= (subs (str '(nil)) 1 4) (.toLowerCase (str a)))) true
             (and (= (str a) "()") (= b nil)) true
             (and (= (str b) "()") (= a nil)) true
             (and (= (str a) "()") (not (= nil b)) (= (subs (str '(nil)) 1 4) (.toLowerCase (str b)))) true
             (and (= (str b) "()") (not (= nil a)) (= (subs (str '(nil)) 1 4) (.toLowerCase (str a)))) true
             true false
     )
) 


(defn igual? [a b]
    (or (= a b)
        (and (nil? a) (= (.toUpperCase (str b)) "NIL"))
        (and (= (.toUpperCase (str a)) "NIL") (nil? b))
        (and (seq? a) (empty? a) (nil? b))
        (and (nil? a) (seq? a) (empty? b))
        (and (seq? a) (empty? a) (= (.toUpperCase (str b)) "NIL"))
        (and (= (.toUpperCase (str a)) "NIL") (seq? b) (empty? b))
        (= (.toUpperCase (str a)) (.toUpperCase (str b))))
)

(defn igual? [a b] (
        cond 
             (= a b) true
             (and (= a nil ) (not (= nil b)) (= (subs (str '(nil)) 1 4) (.toLowerCase (str b)))) true
             (and (= b nil ) (not (= nil a)) (= (subs (str '(nil)) 1 4) (.toLowerCase (str a)))) true
             (and (= (str a) "()") (= b nil)) true
             (and (= (str b) "()") (= a nil)) true
             (and (= (str a) "()") (not (= nil b)) (= (subs (str '(nil)) 1 4) (.toLowerCase (str b)))) true
             (and (= (str b) "()") (not (= nil a)) (= (subs (str '(nil)) 1 4) (.toLowerCase (str a)))) true
             (= (.toLowerCase (str a)) (.toLowerCase (str b))) true
			 true false
     )
)                                    
; Evalua el cuerpo de una macro COND. Siempre retorna una lista con un resultado y un ambiente.
; Recibe una lista de sublistas (cada una de las cuales tiene una condicion en su 1ra. posicion) y los ambientes global y local.
; Si la lista es nil, el resultado es nil y el ambiente retornado es el global.
; Si no, evalua (con evaluar) la cabeza de la 1ra. sublista y, si el resultado no es nil, retorna el res. de invocar a evaluar-secuencia-en-cond con la cola de esa sublista.
; En caso contrario, sigue con las demas sublistas. 
(defn evaluar-cond [lis amb-global amb-local] (
	if
	(nil? lis)
	(list nil amb-global)
	(
		if
		(igual? nil (first (evaluar (first (first lis)) amb-global amb-local)))
        (evaluar-cond (next lis) amb-global amb-local)
		(evaluar-secuencia-en-cond (first (next lis)) amb-global amb-local)
	)	
))

(defn evaluar-cond [lis amb-global amb-local] 
	(cond (nil? lis) (list nil amb-global)
	      (not (igual? (first (evaluar (ffirst lis) amb-global amb-local)) nil)) (evaluar-secuencia-en-cond (first (next lis)) amb-global amb-local)
		  true (recur (next lis) amb-global amb-local))	
)




'(((equal'a 'b)(setq x 1))((equal 'a 'a)(setq y 2)(setq z 3)))

(defn evaluar-secuencia-en-cond [lis amb-global amb-local] 
	(  if
		(nil? (next lis))
		(evaluar (first lis) amb-global amb-local)
		(recur (next lis) (fnext (evaluar (first lis) amb-global amb-local)) amb-local))
)