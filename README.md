# Haskell
<h2> Tableaux  </h2>
<p> Desenvolvedores: Camila Ferreira, Caubi Loureiro </p>

<h3> :computer: Objetivo do trabalho: </h3>
<p> O trabalho é pensado para : </p> 
<li> Promover o contato com uma linguagem funcional (Haskell) </li>
<li> Seguir a lógica clássica proposicional para desenvolver um Tableaux. </li>

<h3> :pencil2: Como utilizar? </h3>
<h4> Comandos necessários : </h4>

```
make compile
```
```
make run
```
<p> Ou somente: </p>

```
make run
```


<h4> Criando as fórmulas : </h4>
<p> Para criar as fórmulas utiliza-se a notação pré-fixada, dessa forma, os operadores são representados da seguinte maneira: </p>

<table>
  <tr>
    <th>Operador</th>
    <th>Char Correspondente</th>
  </tr>
  <tr>
    <td>And</td>
    <td>&</td>
  </tr>
  <tr>
    <td>Or</td>
    <td>v</td>
  </tr>
  <tr>
    <td>Implication</td>
    <td>></td>
  </tr>
  <tr>
    <td>Not</td>
    <td>-</td>
  </tr>
  
</table>

<h4> Exemplo de entradas </h4>
<li> Equivalente a : (a -> (a -> (b ->a))) </li>

```
>(a,>(a,>(b,a)))
 ```
 
 
<li> Equivalente a : (b -> (a and (b or a)))  </li>

```
>(b,&(a,v(b,a)))
 ```

<li> Equivalente a : ( Not b -> (a and b)) </li>

```
>(-b,&(a,b))
 ```
<li> Equivalente a : ((Not h) and (g and (h -> (g or (Not h ->g))))) </li>

```
&(-h,&(g,>(h,v(g,>(-h,g)))))
 ```
 
<li> Equivalente a : ((Not a) and (o and (Not a)))  </li>

```
&(-a,&(o,-a))
 ```



<h3> :pushpin: Resultados: </h3>
<p> O programa possui as seguintes funções : </p>
<li> A partir de uma string, recebida pelo usuário, monta uma árvore  com raiz rotulada por uma fórmula na lógica do Tableaux. </li>
<li> Determina os ramos abertos e os ramos fechados da árvore. </li>
<li> Apresenta a árvore para o usuário. </li>
<li> Classifica a árvore como tautologia ou falsificável, seguindo as regras do Tableaux. </li>
<li> Apresenta os contra-exemplos caso a classificação da árvore seja falsificável. </li>



