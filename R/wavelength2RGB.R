wavelength2RGB <- function(wavelength){
  # CONST
  gamma        <-   0.80
  intensityMax <-   255

  adjust <- function(colour, factor){
    if(colour == 0.0){
      return(0)
    }
    else{
      return(round(intensityMax * (colour * factor) ^ gamma))
    }
  }

  if(wavelength>=380 && wavelength<440){
    red   <- -(wavelength - 440) / (440 - 380);
    green <- 0.0;
    blue  <- 1.0
  }
  else if(wavelength>=440 && wavelength<490){
    red   <- 0.0;
    green <- (wavelength - 440) / (490 - 440);
    blue  <- 1.0
  }
  else if(wavelength>=490 && wavelength<510){
    red   <- 0.0;
    green <- 1.0;
    blue  <- -(wavelength - 510) / (510 - 490)
  }
  else if(wavelength>=510 && wavelength<580){
    red   <- (wavelength - 510) / (580 - 510);
    green <- 1.0;
    blue  <- 0.0
  }
  else if(wavelength>=580 && wavelength<645){
    red   <- 1.0;
    green <- -(wavelength - 645) / (645 - 580);
    blue  <- 0.0
  }
  else if(wavelength>=645 && wavelength<=780){
    red   <- 1.0;
    green <- 0.0;
    blue  <- 0.0
  }
  else{
    red   <- 0.0;
    green <- 0.0;
    blue  <- 0.0
  }
  # Let the intensity fall off near the vision limits
  if(wavelength>=380 && wavelength<420){
    factor <- 0.3 + 0.7*(wavelength - 380) / (420 - 380);
  }
  else if(wavelength>=420 && wavelength<700){
    factor <- 1.0;
  }
  else if(wavelength>=700 && wavelength<=780){
    factor <- 0.3 + 0.7*(780 - wavelength) / (780 - 700)
  }
  else{
    factor <- 0.0
  }
  R <- adjust(red,   factor);
  G <- adjust(green, factor);
  B <- adjust(blue,  factor)

  return(c(R, G, B))
}
