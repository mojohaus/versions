#!/bin/bash
mvn -Prun-its clean verify -Drat.ignoreErrors=true -Dinvoker.test=it-use-latest-versions-001,it-use-latest-versions-002,it-use-latest-versions-003,it-use-latest-versions-004,it-use-latest-versions-005,it-use-latest-versions-006
