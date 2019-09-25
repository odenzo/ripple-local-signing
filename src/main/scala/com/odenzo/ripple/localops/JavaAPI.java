package com.odenzo.ripple.localops;

import io.circe.Json;

/**
 * JavaAPI for the message based interfaces
 */
public class JavaAPI {


    /**
     * @param rq Json as String for iterop, represents the Ripple Request method.
     * @return sign response or throws an exception. Most errors in a negative response message
     *  @throws Exception on json parsing
     */
   public static String sign(String rq) {
        Json json = RippleLocalAPI.parseJsonUNSAFE(rq);
        Json jsonRs = MessageBasedAPI.sign(json);
        return jsonRs.spaces4();
    }

    /**
     * @param rq Json as String for iterop, represents the Ripple Request method.
     * @return sign response or throws an exception. Most errors in a negative response message
     *  * @throws Exception on json parsing
     */
  public static String signFor(String rq) {
        Json json = RippleLocalAPI.parseJsonUNSAFE(rq);
        Json jsonRs = MessageBasedAPI.signFor(json);
        return jsonRs.spaces4();
    }

    /**
     * @param rq Json as String for iterop, represents the Ripple Request method.
     * @return sign response or throws an exception. Most errors in a negative response message
     * @throws Exception on json parsing
     */
  public  static String walletPropose(String rq) {
        Json json = RippleLocalAPI.parseJsonUNSAFE(rq);
        Json jsonRs = MessageBasedAPI.walletPropose(json);
        return jsonRs.spaces4();
    }


}
