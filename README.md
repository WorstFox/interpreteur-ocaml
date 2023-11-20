# Fouine PROFON by Ulysse and Benjamin

Pour compiler, faites
make
qui aura pour effet de créer l'exécutable appelé fouine.
Faites ensuite
./fouine
pour lancer l'exécutable et saisir une expression au clavier. Par exemple
```
> ./fouine
prInt (3+2*5)
12
```

Vous pouvez aussi faire
./fouine tests/basic.ml
pour lancer fouine sur le fichier basic.ml


## Options

- -showsrc permet d'afficher le code source dans deux formats : un format qui correspond à la représentation interne du code, non compréhensible par OCaml et un qui est compréhensible par OCaml, non nécessairement égal au code rentré.
- -debug permet d'afficher ce qu'affiche -showsrc mais affiche aussi la sortie du programme executé sous ces deux mêmes formats.

## Fichiers
main.ml : fichier principal
expr.ml : définition des expressions et de l'évaluation
affichage.ml : fonctions d'affichage
lexer.mll : lexèmes, analyse lexicale
parser.mly : règles de grammaire, analyse syntaxique
tests/ : sous-répertoire de tests
Makefile : pour la compilation, à ne pas modifier a priori

Pour les autres fichiers .ml, ils correspondent aussi à des parties du programme, leur nom est explicite.

Le dossier Autotest permet de tester fouine, il faut se rendre dans ce dossier puis executer
```
./autotest.sh A ../fouine
```


## DONE
Les questions suivantes (dans sujets/rendu1.pdf)  ont été traitées par :

- Ulysse : 1,2,3,4,6,8

- Benjamin : 1,2,5,7,9,11, et les liste.


## TODO
- Ulysse : 10
