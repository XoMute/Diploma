#!/bin/bash
curl http://api.open-notify.org/astros.json -s | \
	hson -f ".people[] | .name"            | \
	sed -E 's#"(.*)"#Astronaut \1 is in space now.#'
