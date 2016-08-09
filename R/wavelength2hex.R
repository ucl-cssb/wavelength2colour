wavelength2hex <- function(wavelength){
  RGB <- wavelength2RGB(wavelength)
  hex <- rgb(RGB[1], RGB[2], RGB[3], maxColorValue = 255)
  return(hex)
}
