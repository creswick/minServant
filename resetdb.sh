#!/bin/bash

dropdb servant
createdb servant
psql -d servant -f schema.sql
