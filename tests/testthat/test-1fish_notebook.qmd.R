# Vérifications de fish_notebook.qmd
fish <- parse_rmd(
  "../../fish_notebook.qmd",
  allow_incomplete = TRUE,
  parse_yaml = TRUE
)

test_that("Le bloc-notes est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("fish_notebook.qmd"))
  # La version compilée HTML du carnet de notes est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.

  expect_true(is_rendered_current("fish_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document est-elle conservée ?", {
  expect_true(all(
    c(
      "Introduction et but",
      "Matériel et méthodes",
      "Résultats",
      "Étude descriptive",
      "Regroupement par CAH",
      "CAH 1",
      "CAH 2",
      "Comparaison des deux regroupements CAH",
      "Comparaison entre les CAH et la variable species",
      "Regroupement par les k-moyennes",
      "Préparation des données",
      "K-moyennes",
      "Comparaison entre les k-moyennes et la CAH 2",
      "Comparaison entre les k-moyennes et la variable species",
      "Discussion et conclusions",
      "Benchmark"
    ) %in%
      (rmd_node_sections(fish) |> unlist() |> unique())
  ))
  # Les sections (titres) attendues du bloc-notes ne sont pas toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).

  expect_true(all(
    c(
      "setup",
      "ai_comment",
      "import",
      "tab",
      "skim",
      "skimcomment",
      "plot1",
      "plot1comment",
      "cluster1",
      "dendro1",
      "dendro1comment",
      "cluster2",
      "dendro2",
      "dendro2comment",
      "fishcah",
      "compacah",
      "compacahcomment",
      "plotcah1",
      "plotcah2",
      "plotcahcomment",
      "compacah1sp",
      "compacah2sp",
      "compacahcomment",
      "prepakmn",
      "profilek",
      "kmean",
      "plotkmn",
      "plotkmncomment",
      "fishcahkmn",
      "compacahkmn",
      "compacahkmncomment",
      "compaspkmn",
      "compaspkmncomment",
      "discu",
      "benchmark",
      "benchmarkcomment"
    ) %in%
      rmd_node_label(fish)
  ))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).

  expect_true(any(duplicated(rmd_node_label(fish))))
  # Un ou plusieurs labels de chunks sont dupliqués
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété ?", {
  expect_true(fish[[1]]$author != "___")
  expect_true(!grepl("__", fish[[1]]$author))
  expect_true(grepl("^[^_]....+", fish[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.

  expect_true(grepl("[a-z]", fish[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.

  expect_true(grepl("[A-Z]", fish[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunks 'import' : importation et filtre des données", {
  expect_true(is_identical_to_ref("import", "names"))
  # Les colonnes dans le tableau `fish` importé ne sont pas celles attendues
  # Votre jeu de données de départ n'est pas correct. Ce test échoue si vous
  # n'avez pas bien rempli le code du chunk 'import'.

  expect_true(is_identical_to_ref("import", "classes"))
  # La nature des variables (classe) dans le tableau `fish` est incorrecte
  # Vérifiez le chunk d'importation des données `import`.

  expect_true(is_identical_to_ref("import", "nrow"))
  # Le nombre de lignes dans le tableau `fish` est incorrect
  # Le filtre sur les lignes n'est pas correcte. Rélisez la consigne pour
  # appliquer le filtre souhaité sur l'altitude au quel les arbres sont mesurés.
})

test_that("Chunks 'tab', 'skim', 'skimcomment' : description des données", {
  expect_true(is_identical_to_ref("tab"))
  # Le tableau produit par le chunk 'tab' n'est pas celui attendu. Avez-vous
  # bien réalisé la variance `headtail` du tableau `tabularise() ?

  expect_true(is_identical_to_ref("skimcomment"))
  # L'interprétation des tableaux fescriptifs des données est (partiellement)
  # fausse dans le chunk 'skimcomment'
  # Vous devez cochez les phrases qui décrivent les données d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'plot1', 'plot1comment' : description graphiques des données", {
  expect_true(is_identical_to_ref("plot1"))
  # Le graphique produit par le chunk 'plot1' n'est pas
  # celui attendu. Avez-vous utilisé un label en français pour l'axe Y des
  # deux graphiques ?

  expect_true(is_identical_to_ref("plot1comment"))
  # L'interprétation de la description graphique des données est (partiellement)
  # fausse dans le chunk 'plot1comment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'cluster1', 'dendro1', 'dendro1comment' : CAH 1", {
  expect_true(is_identical_to_ref("cluster1"))
  # Le cluster CAH obtenu par le chunk 'cluster1' n'est pas celui attendu.
  # Avez-vous éliminé la variable species avant de fournir vos données à
  # dissimilarity() ?
  # Avez-vous utilisé la méthode "manhattan" et standardisé les données
  # (argument `scale =`) dans le calcul de la matrice de distances ?
  # Avez-vous utilisé la méthode "ward.D2" dans cluster() ?
  # Enfin, avez-vous bien assigné le résultat à `fish_clust1`?

  expect_true(is_identical_to_ref("dendro1"))
  # Le dendrogramme produit par le chunk 'dendro1' n'est pas celui attendu.
  # Si le test précédent a réussi, votre graphique doit être amélioré. Utilisez
  # la check-list ci-dessous :
  # Avez vous bien indiqué que vous ne vouliez pas afficher les labels des
  # observations dans chart() avec `labels = FALSE` ?
  # Avez-vous utilisé un label en français pour l'axe Y ("Hauteur") ?
  # Avez-vous ajouté la ligne de coupure en rouge avec geom_dendroline() ?
  # Cette ligne de coupure est-elle bien placée de telle manière qu'elle sépare
  # le jeu de données en cinq groupes ?
  # Le niveau de coupe que nous avons employé est une hauteur de 18.

  expect_true(is_identical_to_ref("dendro1comment"))
  # L'interprétation du premier dendrogramme est (partiellement) fausse dans le
  # chunk 'dendro1comment'
  # Vous devez cochez les phrases qui décrivent le dendrogramme d'un 'x' entre
  # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'cluster2', 'dendro2', 'dendro2comment' : CAH 2", {
  expect_true(is_identical_to_ref("cluster2"))
  # Le cluster CAH obtenu par le chunk 'cluster2' n'est pas celui attendu.
  # Avez-vous éliminé la variable species avant de fournir vos données à
  # dissimilarity() ?
  # Avez-vous utilisé la méthode "euclidean" et standardisé les données
  # (argument `scale =`) dans le calcul de la matrice de distances ?
  # Avez-vous utilisé la méthode "complete" dans cluster() ?
  # Enfin, avez-vous bien assigné le résultat à `fish_clust2`?

  expect_true(is_identical_to_ref("dendro2"))
  # Le dendrogramme produit par le chunk 'dendro2' n'est pas celui attendu.
  # Si le test précédent a réussi, votre graphique doit être amélioré. Utilisez
  # la check-list ci-dessous :
  # Avez vous bien indiqué que vous ne vouliez pas afficher les labels des
  # observations dans chart() avec `labels = FALSE` ?
  # Avez-vous utilisé un label en français pour l'axe Y ("Hauteur") ?
  # Avez-vous ajouté la ligne de coupure en rouge avec geom_dendroline() ?
  # Cette ligne de coupure est-elle bien placée de telle manière qu'elle sépare
  # le jeu de données en sept groupes ?
  # Le niveau de coupe que nous avons employé est une hauteur de 3.

  expect_true(is_identical_to_ref("dendro2comment"))
  # L'interprétation du second dendrogramme est (partiellement) fausse dans le
  # chunk 'dendro2comment'
  # Vous devez cochez les phrases qui décrivent le dendrogramme d'un 'x' entre
  # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunk 'fishcah' : ajout des groupes CAH 1 & 2 aux données", {
  expect_true(is_identical_to_ref("fishcah", "names"))
  # Les colonnes dans le tableau `fish_cah` ne sont pas celles attendues.
  # Vous devez rajouter deux colonnes nommées `cah1` et `cah2` contenant les
  # regroupements des deux CAH.
  # Avez-vous bien renommé correctement les colonnes issues de augment() ? Elle
  # s'appelle `.fitted` par défaut.

  expect_true(is_identical_to_ref("fishcah", "classes"))
  # La nature des variables (classe) dans le tableau `fish_cah` est incorrecte.
  # Vérifiez que vous avez converti `cah1` et `cah2` en variables `factor`.

  expect_true(is_identical_to_ref("fishcah", "nrow"))
  # Le nombre de lignes dans le tableau `fish_cah` est incorrect.
  # En principe, le traitement effectué ne modifie pas le nombre de lignes par
  # rapport au tableau `fish`. Vérifiez votre code.
})

test_that("Chunks 'compacah', 'compacahcomment' : comparaison des deux CAH", {
  expect_true(is_identical_to_ref("compacah", "class"))
  # Le tableau de contingence produit par le chunk 'compacah' n'est pas de la
  # classe attendue.
  # Assurez-vous d'utiliser la fonction table() pour générer ce tableau de
  # contingence.

  expect_true(is_identical_to_ref("compacah", "dim"))
  # Le tableau de contingence produit par le chunk 'compacah' n'a pas la taille
  # attendue.
  # Soit vous avez inversé les deux CAH (lignes vs colonnes), soit vous n'avez
  # pas indiqué les bons niveaux de coupure plus haut dans le chunk 'fishcah',
  # soit encore, vous utilisez les mauvaises données. Relisez l'énoncé en haut
  # du chunk 'compacah'.

  expect_true(is_identical_to_ref("compacah", "dimnames"))
  # Le tableau de contingence produit par le chunk 'compacah' n'a pas les noms
  # de lignes et de colonnes attendus.
  # Assurez-vous de nommer vos deux arguments comme indiqué dans la consigne au
  # dessus du chunk 'compacah'.

  expect_true(is_identical_to_ref("compacahcomment"))
  # L'interprétation du tableau de contingence est (partiellement) fausse dans
  # le chunk 'compacahcomment'.
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'plotcah1', 'plotcah2', 'plotcahcomment' : graphiques avec groupes CAH 1 & 2", {
  expect_true(is_identical_to_ref("plotcah1"))
  # Le graphique produit par le chunk 'plotcah1' n'est pas celui attendu.
  # Avez-vous réalisé un nuage de points avec chart() et geom_point() ?
  # Avez-vous utilisé les bonnes variables pour les axes X, Y et la couleur ?
  # Relisez la consigne au dessus du chunk 'plotcah1' pour vous en assurer.

  expect_true(is_identical_to_ref("plotcah2"))
  # Le graphique produit par le chunk 'plotcah2' n'est pas celui attendu.
  # Avez-vous réalisé un nuage de points avec chart() et geom_point() ?
  # Avez-vous utilisé les bonnes variables pour les axes X, Y et la couleur ?
  # Relisez la consigne au dessus du chunk 'plotcah2' pour vous en assurer.

  expect_true(is_identical_to_ref("plotcahcomment"))
  # L'interprétation des graphiques avec groupes CAH 1 & 2 est (partiellement)
  # fausse dans le chunk 'plotcahcomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'compacah1sp', 'compacah2sp', 'compacahspcomment' : comparaison des CAH avec les espèces", {
  expect_true(is_identical_to_ref("compacah1sp", "class"))
  # Le tableau de contingence produit par le chunk 'compacah1sp' n'est pas de la
  # classe attendue.
  # Assurez-vous d'utiliser la fonction table() pour générer ce tableau de
  # contingence.

  expect_true(is_identical_to_ref("compacah1sp", "dim"))
  # Le tableau de contingence produit par le chunk 'compacah1sp' n'a pas la
  # taille attendue.
  # Soit vous avez inversé les espèces et la CAH (lignes vs colonnes), soit vous
  # n'avez pas indiqué les bons niveaux de coupure plus haut dans le chunk
  # 'fishcah', soit encore, vous utilisez les mauvaises données. Relisez
  # l'énoncé en haut du chunk 'compacah1sp'.

  expect_true(is_identical_to_ref("compacah1sp", "dimnames"))
  # Le tableau de contingence produit par le chunk 'compacah1sp' n'a pas les
  # noms de lignes et de colonnes attendus.
  # Assurez-vous de nommer vos deux arguments comme indiqué dans la consigne au
  # dessus du chunk 'compacah1sp'.

  expect_true(is_identical_to_ref("compacah2sp", "class"))
  # Le tableau de contingence produit par le chunk 'compacah2sp' n'est pas de la
  # classe attendue.
  # Assurez-vous d'utiliser la fonction table() pour générer ce tableau de
  # contingence.

  expect_true(is_identical_to_ref("compacah2sp", "dim"))
  # Le tableau de contingence produit par le chunk 'compacah2sp' n'a pas la
  # taille attendue.
  # Soit vous avez inversé les espèces et la CAH (lignes vs colonnes), soit vous
  # n'avez pas indiqué les bons niveaux de coupure plus haut dans le chunk
  # 'fishcah', soit encore, vous utilisez les mauvaises données. Relisez
  # l'énoncé en haut du chunk 'compacah2sp'.

  expect_true(is_identical_to_ref("compacah2sp", "dimnames"))
  # Le tableau de contingence produit par le chunk 'compacah2sp' n'a pas les
  # noms de lignes et de colonnes attendus.
  # Assurez-vous de nommer vos deux arguments comme indiqué dans la consigne au
  # dessus du chunk 'compacah2sp'.

  expect_true(is_identical_to_ref("compacahspcomment"))
  # L'interprétation du tableau de contingence est (partiellement) fausse dans
  # le chunk 'compacahspcomment'.
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunk 'prepakmn' : préparation des données pour les k-moyennes", {
  expect_true(is_identical_to_ref("prepakmn", "names"))
  # Les colonnes dans le tableau `fish_scaled` ne sont pas celles attendues.
  # Vous devez éliminer la colonne `species` de `fish`.

  expect_true(is_identical_to_ref("prepakmn", "classes"))
  # La nature des variables (classe) dans le tableau `fish_scaled` est incorrecte.
  # Vérifiez que vous avez standardisé les données avec scale().

  expect_true(is_identical_to_ref("prepakmn", "nrow"))
  # Le nombre de lignes dans le tableau `fish_scaled` est incorrect.
  # En principe, le traitement effectué ne modifie pas le nombre de lignes par
  # rapport au tableau `fish`. Vérifiez votre code.
})

test_that("Chunks 'profilek', 'kmean', 'plotkmn', 'plotkmncomment' : regroupement par les k-moyennes", {
  expect_true(is_identical_to_ref("profilek"))
  # Le graphique de profilage de k n'est pas réalisé ou n'est pas celui attendu.
  # Vérifiez que vous utilisez la fonction profile_k() sur vos données
  # standardisées `fish_scaled`.

  expect_true(is_identical_to_ref("kmean"))
  # Le regroupement par les k-moyennes ne donne pas les groupes attendus.
  # Vérifiez que vous utilisez les données standardisées.
  # Vérifiez que vous réalisez un regroupement en sept groupes.
  # Assurez-vous de prendre suffisamment de points de départ (argument
  # `nstart =`) pour converger vers la solution. Relisez la consigne au dessus
  # du chunk 'kmean' pour vous assurer que cela est correct.
  # Vérifiez que vous utilisez un set.seed() de 32763 avant k_means().

  expect_true(is_identical_to_ref("plotkmn"))
  # Le graphique des k-moyennes n'est pas fait ou n'est pas celui attendu.
  # Vous devez utiliser chart() sur votre objet `fish_kmn`.
  # Vous devez indiquer dans l'argument `choices =` un vecteur de deux entiers
  # reprenant les numéros des colonnes dans `fish_scaled` qui correspondent à la
  # longueur totale standardisée et à la masse standardisée.

  expect_true(is_identical_to_ref("plotkmncomment"))
  # L'interprétation du graphique des k-moyennes est (partiellement) fausse dans
  # le chunk 'plotkmncomment'.
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'fishcahkmn', 'compacahkmn', 'compacahkmncomment' : comparaison k-moyenne et CAH 2", {
  expect_true(is_identical_to_ref("fishcahkmn", "names"))
  # Les colonnes dans le tableau `fish_cah_kmn` ne sont pas celles attendues.
  # Vous devez rajouter une colonne nommée `kmeans` contenant le regroupement de
  # votre k-moyenne à `fish_cah`.
  # Avez-vous bien renommé correctement la colonne issue de augment() ? Elle
  # s'appelle `.cluster` par défaut.

  expect_true(is_identical_to_ref("fishcahkmn", "classes"))
  # La nature des variables (classe) dans le tableau `fish_cah_kmn` est
  # incorrecte.
  # Vérifiez que `cah1`, `cah2` et `kmeans` sont des variables `factor`.

  expect_true(is_identical_to_ref("fishcahkmn", "nrow"))
  # Le nombre de lignes dans le tableau `fish_cah_kmn` est incorrect.
  # En principe, le traitement effectué ne modifie pas le nombre de lignes par
  # rapport au tableau `fish`. Vérifiez votre code.

  expect_true(is_identical_to_ref("compacahkmn", "class"))
  # Le tableau de contingence produit par le chunk 'compacahkmn' n'est pas de la
  # classe attendue.
  # Assurez-vous d'utiliser la fonction table() pour générer ce tableau de
  # contingence.

  expect_true(is_identical_to_ref("compacahkmn", "dim"))
  # Le tableau de contingence produit par le chunk 'compacahkmn' n'a pas la
  # taille attendue.
  # Soit vous avez inversé cah2 et kmeans (lignes vs colonnes), soit vous n'avez
  # pas indiqué les bons niveaux de coupure plus haut dans le chunk 'fishcah' ou
  # le bon nombre de groupes dans le chunk 'kmean', soit encore, vous utilisez
  # les mauvaises données. Relisez l'énoncé en haut du chunk 'compacahkmn'.

  expect_true(is_identical_to_ref("compacahkmn", "dimnames"))
  # Le tableau de contingence produit par le chunk 'compacahkmn' n'a pas les
  # noms de lignes et de colonnes attendus.
  # Assurez-vous de nommer vos deux arguments comme indiqué dans la consigne au
  # dessus du chunk 'compacahkmn'.

  expect_true(is_identical_to_ref("compacahkmncomment"))
  # L'interprétation du tableau de contingence est (partiellement) fausse dans
  # le chunk 'compacahkmncomment'.
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'compaspkmn', 'compaspkmncomment' : comparaison k-moyennes et variable species", {
  expect_true(is_identical_to_ref("compaspkmn", "class"))
  # Le tableau de contingence produit par le chunk 'compaspkmn' n'est pas de la
  # classe attendue.
  # Assurez-vous d'utiliser la fonction table() pour générer ce tableau de
  # contingence.

  expect_true(is_identical_to_ref("compaspkmn", "dim"))
  # Le tableau de contingence produit par le chunk 'compaspkmn' n'a pas la
  # taille attendue.
  # Soit vous avez inversé species et kmeans (lignes vs colonnes), soit vous
  # n'avez pas indiqué le bon nombre de groupes dans le chunk 'kmean', soit
  # encore, vous utilisez les mauvaises données. Relisez l'énoncé en haut du
  # chunk 'compaspkmn'.

  expect_true(is_identical_to_ref("compaspkmn", "dimnames"))
  # Le tableau de contingence produit par le chunk 'compaspkmn' n'a pas les
  # noms de lignes et de colonnes attendus.
  # Assurez-vous de nommer vos deux arguments comme indiqué dans la consigne au
  # dessus du chunk 'compaspkmn'.

  expect_true(is_identical_to_ref("compaspkmncomment"))
  # L'interprétation du tableau de contingence est (partiellement) fausse dans
  # le chunk 'compaspkmncomment'.
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'discu', 'benchmarkcomment' : discussion et conclusions", {
  expect_true(is_identical_to_ref("discu"))
  # Les items sélectionnés dans la discussion sont (partiellement) faux dans le
  # chunk 'discu'.
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_identical_to_ref("benchmarkcomment"))
  # L'interprétation du benchmark est (partiellement) fausse dans le chunk
  # 'benchmarkcomment'.
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})
