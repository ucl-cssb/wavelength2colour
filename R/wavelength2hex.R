#' Turn a wavelength (in nm) in to a hex colour value.
#'
#' Converts a wavelength to a hex value, via RGB.
#'
#' @param wavelength a numerical wavelength to convert to hex.
#'
#' @return a hex representation of the wavelength.
#' @export
wavelength2hex <- function(wavelength){
  RGB <- wavelength2RGB(wavelength)
  hex <- grDevices::rgb(RGB[1], RGB[2], RGB[3], maxColorValue = 255)
  return(hex)
}
