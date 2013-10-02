// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// ZigguratGSL.h:  Jochen Voss's Ziggurat implementation from the GNU GSL
//
// Copyright (C) 2013  Dirk Eddelbuettel
//
// This file is part of RcppZiggurat.
//
// RcppZiggurat is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// RcppZiggurat is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with RcppZiggurat.  If not, see <http://www.gnu.org/licenses/>.

// Ziggurat using Jochen Voss's version in the GSL

#ifndef RcppZiggurat__ZigguratGSL_h
#define RcppZiggurat__ZigguratGSL_h

#include <RcppGSL.h>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

class ZigguratGSL {
public: 
    ZigguratGSL() {
	gsl_rng_env_setup() ;
	r = gsl_rng_alloc (gsl_rng_default);
    }
    ~ZigguratGSL() {
	gsl_rng_free(r);
    }
    double norm(const double sigma=1.0) {
	return gsl_ran_gaussian_ziggurat(r, sigma);
    }
    // could use functions to set/get seed etc
private:
    gsl_rng *r;
};

#endif
