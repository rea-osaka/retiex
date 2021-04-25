#' add assumed land unit price col
#'
#' add land unit price col which was calculated
#' by assuming building price and
#' subtracting assumed building price from total price.
#'
#' @param reti_data data.frame
#' @param building_unit_cost Int
#' @param building_durability Int
#' @param heighzone string vector
#'
#' @return data.frame
#' @export
#'
retiex_add_assumption_land_unit_price <-
  function(reti_data,
           building_unit_cost = 200000,
           building_durability = 40,
           heighzone = NULL){


    reti_data %>%
    dplyr::mutate(

      individual_volume_rate =
        local_calc_individual_volume_rate(`容積率`,`道路幅員`,`都市計画`,heighzone),

      assumption_building_size =
        ifelse(huge_building, land_size * individual_volume_rate, building_size),

      assumption_land_unit_price =
        (`取引総額` - local_calc_building_price(building_unit_cost,
                                     assumption_building_size,
                                     building_durability,
                                     howold_building)) / land_size
    )
}


##################################################################################
# ローカル関数
##################################################################################

local_calc_individual_volume_rate <- function(general_vol, road_w, zone, heighzone = NULL){

  # 係数0.6と0.4を地域によって確定
  # 追加がある場合は引数で取る
  if(is.null(heighzone)){
    heighzone <- c("商業地域","近隣商業地域","工業専用地域","工業地域","準工業地域")
  }else{
    heighzone <- c(heighzone,"商業地域","近隣商業地域","工業専用地域","工業地域","準工業地域")
  }

  road_w <- ifelse(road_w < 4, 4, road_w)
  applied_rate <- ifelse(zone %in% heighzone, 0.6, 0.4)

  ans <- ifelse(general_vol / 100 > road_w * applied_rate,
                road_w * applied_rate,
                general_vol / 100)
  return(ans)
}


local_calc_building_price <- function(unit_cost, size, durability, howold){
  rest_term <- ifelse(durability - howold < 0, 0, durability - howold)

  unit_cost * size * (rest_term / durability)
}


