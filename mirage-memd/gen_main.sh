#!/bin/sh -e

echo open Server_memd >> main.ml
echo "let _ = OS.Main.run (main ())" >> main.ml
