package org.cardano.govtool.simulations;

import java.util.Optional;

public class SimulationConfig {
    public static final String API_URL = Optional.ofNullable(System.getenv("API_URL")).orElse("https://govtool.cardanoapi.io/api");
    public static final String METADATA_VALIDATION_API_URL = Optional.ofNullable(System.getenv("METADATA_VALIDATION_API_URL")).orElse(API_URL);
    public static final String PDF_API_URL = Optional.ofNullable(System.getenv("PDF_API_URL"))
                        .orElse(API_URL);
    public static final int PEAK_USERS = Integer.parseInt(Optional.ofNullable(System.getenv("PEAK_USERS")).orElse("600"));
    public static final int STRESS_DURATION = Integer.parseInt(Optional.ofNullable(System.getenv("STRESS_DURATION")).orElse("20"));
    public static final int RAMP_DURATION = Integer.parseInt(Optional.ofNullable(System.getenv("RAMP_DURATION")).orElse("20"));
    public static String metadataUrl(String url){
      if(API_URL == METADATA_VALIDATION_API_URL){
        return url;
      }
      return createUrl(METADATA_VALIDATION_API_URL, url);
    }
    public static String apiUrl(String url){
      return createUrl(API_URL, url);
    }

    public static String createUrl(String baseUrl,String url){
      if (baseUrl.endsWith("/")){
          if(url.startsWith("/")){
            return baseUrl + url.substring(1);
          }else{
            return baseUrl  + url;
          }
      }else{
          if(url.startsWith("/")){
            return baseUrl + url;
          }else{
            return baseUrl  + "/" + url;
          }
        }
    }
}
