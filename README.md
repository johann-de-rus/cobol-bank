# COBOL Bank Processor

Petit projet pour apprendre COBOL avec GnuCOBOL.

## Objectif

Lire un fichier de transactions bancaires (CSV) et calculer le solde de chaque compte.

## Exemple de données

account_id,amount,type
1,100,deposit
1,50,withdraw

## Fonctionnalités

- lecture de fichier CSV
- parsing avec UNSTRING
- gestion de transactions (deposit / withdraw)
- calcul de soldes

## Lancer le projet

```bash
cobc -x -o main main.cob
./main