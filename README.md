# gargallica

Programmes R d'interfaces avec l'API de gallica




### Collecter les réponses de l'API gallica en XML, et convertir en csv

Le programme principal, `gallica.R` permet de récupérer toutes les réponses à une question sur gallica.bnf.fr au format XML. 

- la fonction `page` permet de récupérer 50 réponses (une page), et en récupérant automatiquement le nombre total de réponses via l'API, il est possible de boucler jusqu'à la récolte de toutes les réponses. 
- le format XML hiérarchique de l'API permet d'agréger les réponses de toutes les pages dans un seul XML.

La fonction `parse_gallica` permet de convertir ce format hiérarchique en tableau, puis de l'exporter en csv.

### Collecter les textes bruts OCR via l'API gallica

Ici, la fonction `texteBrut` interroge directement la page html de gallica qui affiche le résultat de l'OCR sur un document identifié par son URL. Pour l'exemple dans ce projet, on a filtré sur les documents dont le score OCR (nqamoyen) est supérieur à 80.


Vu le volume de mots récupérés, il peut être utile d'effectuer un fenêtrage autour de certains mots d'intérêt (200 mots avant une expression et 200 mots après), c'est ce que permet la fonction `fenetrage`. Cette étape est assez couteuse pour l'ordinateur et utilise le package tidytext.


### Packages utilisés : 


- tidyverse
- rvest
- xml2
- tidytext
- purrr
- ggraph, igraph (non indispensable)

### Un exemple 

Exemple : 

- page d'un utilisateur qui visite le site : https://gallica.bnf.fr/ark:/12148/bpt6k6277519m

#### Données récupérées

```{r}
knitr::kable(tot_df %>% 
filter(identifier == "http://gallica.bnf.fr/ark:/12148/bpt6k6277519m") %>% 
  tidyr::gather(variable, valeur))
```

|variable    |valeur                                                                                                                    |
|:-----------|:-------------------------------------------------------------------------------------------------------------------------|
|recordId    |8                                                                                                                         |
|contributor |Ponson du Terrail, Pierre Alexis de (1829-1871). Directeur de publication                                                 |
|creator     |                                                                                                                          |
|date        |1862-06-19                                                                                                                |
|description |19 juin 1862 -- 1862/06/19 (A2,N48). --                                                                                   |
|format      |Nombre total de vues : 1290                                                                                               |
|identifier  |http://gallica.bnf.fr/ark:/12148/bpt6k6277519m                                                                            |
|language    |fre -- français                                                                                                           |
|publisher   |[s.n.] (Paris)                                                                                                            |
|relation    |Notice du catalogue : http://catalogue.bnf.fr/ark:/12148/cb32749784n -- http://gallica.bnf.fr/ark:/12148/cb32749784n/date |
|rights      |domaine public -- public domain                                                                                           |
|source      |Bibliothèque nationale de France, département Littérature et art, Z-4876-4878                                             |
|title       |Les Coulisses du monde : journal de M. Ponson du Terrail                                                                  |
|type        |texte -- text -- publication en série imprimée -- printed serial                                                          |


On récupère aussi dans le XML le nqa moyen de l'OCR pour ce document : 96.29.

#### texte Brut 

https://gallica.bnf.fr/ark:/12148/bpt6k6277519m.texteBrut

##### en-tête

Le site affiche une en-tête qui rappelle les informations du XML, ainsi que le score de l'OCR : 

```
identifier,entete
http://gallica.bnf.fr/ark:/12148/bpt6k6277519m,"Rappel de votre demande:
Format de téléchargement: : Texte
Vues 1 à 8 sur 8
Nombre de pages: 8
Notice complète:
Titre : Les Coulisses du monde : journal de M. Ponson du Terrail
Éditeur : [s.n.] (Paris)
Date d'édition : 1862-06-19
Contributeur : Ponson du Terrail, Pierre Alexis de (1829-1871). Directeur de publication
Type : texte
Type : publication en série imprimée
Langue : français
Langue : language.label.français
Format : Nombre total de vues : 1290
Description : 19 juin 1862
Description : 1862/06/19 (A2,N48).
Droits : domaine public
Identifiant : ark:/12148/bpt6k6277519m
Source : Bibliothèque nationale de France, département Littérature et art, Z-4876-4878
Notice du catalogue : http://catalogue.bnf.fr/ark:/12148/cb32749784n
Notice du catalogue : http://gallica.bnf.fr/ark:/12148/cb32749784n/date
Provenance : Bibliothèque nationale de France
Date de mise en ligne : 11/10/2012
Le texte affiché peut comporter un certain nombre d'erreurs. En effet, le mode texte de ce document a été généré de façon automatique par un programme de reconnaissance optique de caractères (OCR). Le taux de reconnaissance estimé pour ce document est de 96 %."
```

##### texte brut (extrait)

On récolte le texte brut issu des traitements OCR de Gallica.

```
L'ÉCOLIER PK 
PARIS 
GRAND ROMAN HISTORIQUE 
DU RÈGNE 
DE LOUIS XI 
TROISIÈME PARTIE 
X (SUITE) Nancy est au due de Lorraine, et c'est le duc de Lorraine qui commandait, les Suisses à la bataille. 
Donc c'est le duc de Lorraine qui a tué ou fait occire, ce qui revient absolument au même, monseigneur Charles de Bourgogne. 
Une sorte d'inquiétude vague se manifesta sur le visage du sire de Bourganeuf. 
- C'est donc au duc que je veux m'en prendre. 
Bourganeuf haussa les épaules avec dédain. Je sais bien, ajouta Rulile, que ce n'est point votre manière de voir, attendu que vous êtes ici tout exprès pour faire adopter le duc René par le roi de Provence, et le marier ensuite à Marie de Bourgogne. 
Qu'en savez-vous ? fit brusquement Bourganeuf. 
Bon ! fit le Napolitain, cela se devine aisément. 
Or, moi, je suis ici pour obtenir un résultat tout contraire. Voilà, cependant, cher seigneur, où conduit la politique: elle fait de vous, l'ami, le dévoué du duc de Bourgogne, le serviteur et l'appui momentané de son ennemi, de son vainqueur, je dirais presque de son meurtrier, René de LOlraine, - et de moi, qui n'ap- 
Albcft Morel aux pieds de Klavie. 
partcnais point au duc, le vengeur de sa mort. Tout 1 cela est fort drôle. 
Et, cette fois, Bufile rompit en visière avec Bourganeuf, en lui tournant tout à fait le dos. 
Sire, disait en ce moment la duchesse de Brancas d'une voix émue, on m'a appris que le duc René de Lorraine devait se rendre à votre cour. 
Bourganeuf tressaillit encore ; il prévoyait un nouveau coup de Bufile et du roi de France par l'organe de la duchesse. 
Eh bien, ma fille, dit le vieux roi, qui tenait toujours dans ses mains amaigries les blanches mains de la duchesse, nous le recevrons. 
Sire, poursuivit madame de Brancas d'une voix 
assurée et calme, je supplierai Votre Majesté, la veille de l'arrivée de ce prince, de vouloir bien me permettre de prendre congé d'elle. 
Et. où irez-vous, ma chère fille ? 
Sire, j'irai pleurer mon époux dans sa terre de Céreste, afin de ne point me trouver face a face avec son meurtrier. 
```

