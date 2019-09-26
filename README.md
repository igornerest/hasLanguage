# Sobre o Projeto
**Processamento de Linguagem Natural** (PLN) é uma subárea da Inteligência Artificial e da Linguística que estuda como a linguagem humana (escrita ou falada) é operada, possibilitando, a partir disso, a construção de mecanismos artificiais ou sistemas que analisem ou gerem linguagem, permitindo um melhor entendimento acerca dela. 

Atualmente existem muitas aplicações já desenvolvidas: *chatterbots*, desde os mais simples, usando a busca de padrões textuais, como a Eliza, até algoritmos que usam probabilidade, como o Markov; normalizadores de texto, entre eles *stemmers* e *lemmatizers*; análise de sentimentos, entre outros. Para linguagens como o Python, muitas ferramentas podem ser facilmente encontradas através da biblioteca NLTK (*Natural Language ToolKit*). Entretanto, quando se fala em Haskell, há uma dificuldade maior ao achar boas bibliotecas, e as ferramentas são encontradas em bibliotecas diferentes. Em resumo, há uma descentralização dos recursos. 

Tendo ciência da deficiência de bibliotecas de PLN em Haskell, surge essa proposta de implementar alguns algoritmos e técnicas clássicas (bases) para o processamento automática da linguagem humana. 

Nesse trabalho, foram implementadas três aplicações: uma adaptação do *chatterbot* Eliza (baseada na biblioteca NLTK do python), um gerador de frases aleatórias usando n-gramas e um classificador de textos. 



# Autoria
O projeto foi desenvolvido pelo aluno Igor Neres Trindade para a disciplina de Paradigmas de Programação, da Universidade Federal do ABC,  ministrada pelo professor [Emilio Francesquini](http://professor.ufabc.edu.br/~e.francesquini/) no 2º quadrimestre de 2019.



# Instalação, teste e execução

Vá ao diretório em que deseja clonar o projeto e digite os seguintes comandos:

```
$ git clone https://github.com/ufabc-bcc/proposta-de-projeto-noturno-haslanguage-nlp-toolkit.git
$ cd cd proposta-de-projeto-noturno-haslanguage-nlp-toolkit/hasLanguage/src
```

Os testes criados podem ser acessados pelo arquivo *Spec.hs*, dentro do diretório *src*. Para rodar os testes, digite o comando:
```
$ stack Spec.hs
```

O projeto consiste em três implementações, cada um em um arquivo.

## Eliza
Eliza foi um dos primeiros sistemas de PLN, descrito em:

**Weizenbaum, Joseph. (1966). ELIZA – A computer program for the study of natural language communication between man and machine. Communications of the ACM, 9(1), 36–45.**

É um *chatterbot* programado para manter conversa bem superficial com um usuário, imitando respostas, através de padrões textuais na conversa. A resposta de é sempre dada através da substituição desses padrões. Cria-se, no fim, uma ilusão de entendimento da conversa.

Para rodar o programa, digite:
```
$ stack Eliza.hs
```
O funcionamento é simples: digite frases **em inglês** e converse com a Eliza. Para sair, **seja educado** e diga *bye*, *goodbye*, *see you* ou *see ya*

## N-gramas

Em linhas gerais, um n-grama é sequência contígua de N elementos. Esses elementos podem ser qualquer coisa! Ao serem obtidos a partir de um corpus, por exemplo, eles podem ser palavras, sílabas ou caracteres. E dependendo do número de elementos contíguos, denominamos o n-grama de uma forma intuitiva: começa-se por unigrama, para uma sequência de um elemento, seguido de bigrama ou digrama, trigrama, 4-grama, 5-grama...

Nesse programa, geramos frases de acordo com uma base de dados. As frases são geradas de maneira a fazer sentido, isto é, verifica-se a probabilidade de uma palavra dada as anteriores na formação da frase. Entretanto, frases diferentes podem ser geradas, pois estipula-se um limiar de probabilidade para cada escolha, evitando ciclos.

Para rodar o programa, digite:
```
$ stack NGrams.hs
```
e estipule o número de palavras. A frase fez sentido?

## Naive Bayes Classifier 

Nessa última aplicação, desenvolvemos um modelo de aprendizado de máquina para classificação de textos. Para isso, foi implementado um  classificador *Naive Bayes*.

Tal classificador, dado um documento *d* qualquer, de todos os rótulos de nosso conjunto de rótulos, retorna a classe *c* com maior **probabilidade a posteriori**, ou seja, com maior probabilidade P( c | d ). 

Aplicando o teorema de Bayes nessa fórmula,  temos que essa probabilidade é dada por $\frac{P( d | c ) P ( c ) }{P( d )}$. Note que, como buscamos a classe que maximiza essa probabilidade e o documento é o mesmo para todos os cálculos de todas as classes, podemos simplesmente ignorar P( d ), calculando somente o produto da verossimilhança P( d | c ) pela probabilidade a priori da classe P( c ). 

Podemos dar um passo extra e, sem perder generalização, representar o documento *d* como um conjunto de palavras *w*. Dessa forma, a **verossimilhança** será dada por P( w1, w2, ... , wn | c ), o que é MUITO custoso para se computar, principalmente com um número grande de palavras. É por esse motivo que usamos o classificador “ingênuo” de Bayes: ele simplifica a fórmula da verossimilhança assumindo que não há dependência das palavras e suas posições e que cada probabilidade P(w | c) é condicionalmente independente. 

Assim, conseguimos reduzir nossa fórmula geral a simplesmente P( w1 | c ) ... P( wn | c )  P( c ). Cada probabilidade dessa pode ser facilmente calculada computacionalmente. A probabilidade da classe *c* a priori, ou P( c ), é dada pela porcentagem de corpora treinados que estão na classe *c*, enquanto que cada probabilidade P( w | c ) da verossimilhança é dada pela quantidade de vezes que a palavra *w* aparece entre todas as palavras contidas nas corpora com rótulo c. É puramente uma contagem! 

Para rodar o programa, digite:
```
$ stack NBClassifier.hs
```
e digite alguma frase. Quanto maior a base de dados, melhor a classificação! Se quiser sair do programa, digite *batman*. O porquê disso? Bem.. todo mundo gosta do batman.

## Sobre o desenvolvimento
Haskell se mostrou uma linguagem difícil no começo, devido ao costume com linguagens estruturadas ou orientadas a objetos. Mas a imutabilidade, a tipagem forte e estática e a clara separação entre funções puras e impuras se tornaram aliados durante o desenvolvimento. O resultado? Apesar de trabalhosa, a implementação do classificador *Naive Bayes* funcionou como deveria sem muitos problemas. Isso evidencia uma famosa frase sobre o Haskell:

> If it compiles, it works.

Além disso, foram utilizados MVars na implementação de Naive Bayes, possibilitando uma implementação futura de concorrência/paralelismo. Nisso, Haskell possui uma outra vantagem: seus programas são fáceis de paralelizar, rodando de maneira eficiente em hardware *multicore*.
