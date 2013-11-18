#!/bin/bash

echo "Compiling the code..."
erl -make
echo "Finished! Please copy the .beam files in ebin/ to the EJABBERD_ROOT/lib/ejabberd/ebin/ directory"


