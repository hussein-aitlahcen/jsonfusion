# Introduction

Nous avons besoin de cartographier un pays pour le supporter dans notre
application. Le cas qui nous intéresse présentement est de pouvoir découper
un pays en zones, généralement inspirées des découpages administratifs.

En France, nous reprenons les départements, en Belgique nous prenons les
provinces, de taille similaires aux départements.

La norme ISO-3166-2 définit la nomenclature des différents découpages
administratifs reconnus pour la plupart des pays. Dans le cas de la Belgique il
existe les régions et les provinces.

Les provinces sont les héritières des départements français datant de la
révolution française (d'où la grande similarité avec nos départements).

Malheureusement, ce découpage ne couvre pas tout le territoire. La capitale
Bruxelles n'est pas un département, mais seulement une région.

Pour construire notre mappage nous avons donc besoin de fusionner les provinces
et la région de Bruxelles.

--
wiki: https://fr.wikipedia.org/wiki/ISO_3166-2:BE
geojson: https://s3.amazonaws.com/osm-polygons.mapzen.com/belgium_geojson.tgz

## Solution

Créer une petite librairie qui permet de fusionner deux fichier JSON.
Le comportement de la fusion est fournit pas l'utilisateur de la librairie.

En l'occurrence, on fusionne la région de de Bruxelles avec les provinces.
Elle est identifiable par son numéro '54094', il est donc assez simple de cibler
cet élément dans le fichier et de l'intégrer aux provinces.

J'ai choisi de séparé le code impure du pure, ainsi que les types manipulés
(la librairie est générique pour n'importe quelle classe FromJSON/ToJSON d'Aeson).

Pour rendre la fusion plus claire, j'utilise des lens, qui me premettent de traverser
la structure et de cibler les éléments que je souhaite modifier (purely, obviously).

Il suffit d'exécuter les commandes suivants pour obtenir le fichier geojson:
`cd lib`
`stack build`
`stack exec -- jsonfusion-exe ./../files/admin_level_6.geojson ./../files/admin_level_4.geojson ./belgium.geojson`
