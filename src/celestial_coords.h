#ifndef CELESTIAL_COORDS_H
#define CELESTIAL_COORDS_H

#include <cmath>

struct CelestialCoords {
  int nL;
  int nR;
  int nX;
  double theta;
  double phi;
  double omega_conformal;
};

inline CelestialCoords update_coords(const CelestialCoords& coords,
                                      int delta_L, int delta_R, int delta_X) {
  CelestialCoords nc;
  nc.nL = coords.nL + delta_L;
  nc.nR = coords.nR + delta_R;
  nc.nX = coords.nX + delta_X;

  double r = std::sqrt((double)(nc.nL * nc.nL + nc.nR * nc.nR + nc.nX * nc.nX));
  nc.omega_conformal = r;

  if (r > 1e-10) {
    nc.theta = std::acos(nc.nX / r);
    nc.phi = std::atan2((double)nc.nR, (double)nc.nL);
  } else {
    nc.theta = 0.0;
    nc.phi = 0.0;
  }
  return nc;
}

inline CelestialCoords create_empty_coords() {
  CelestialCoords c;
  c.nL = 0; c.nR = 0; c.nX = 0;
  c.theta = 0.0; c.phi = 0.0; c.omega_conformal = 0.0;
  return c;
}

#endif // CELESTIAL_COORDS_H
