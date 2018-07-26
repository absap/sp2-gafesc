CLASS /dmo/cl_flight_legacy DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS /dmo/cl_flight_data_generator.

  PUBLIC SECTION.
    INTERFACES: /dmo/if_flight_legacy.

    TYPES: BEGIN OF ENUM ty_change_mode STRUCTURE change_mode," Key checks are done separately
             create,
             update," Only fields that have been changed need to be checked
           END OF ENUM ty_change_mode STRUCTURE change_mode.

    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO /dmo/cl_flight_legacy.

    "   With respect to the same method call of create/update/delete_travel() we have All or Nothing.
    "   I.e. when one of the levels contains an error, the complete call is refused.
    "   However, the buffer is not cleared in case of an error.
    "   I.e. when the caller wants to start over, he needs to call Initialize() explicitly.

    METHODS set_status_to_booked IMPORTING iv_travel_id            TYPE /dmo/travel_id
                                 EXPORTING et_messages             TYPE /dmo/if_flight_legacy=>tt_if_t100_message.

    METHODS create_travel IMPORTING is_travel             TYPE /dmo/if_flight_legacy=>ts_travel_in
                                    it_booking            TYPE /dmo/if_flight_legacy=>tt_booking_in OPTIONAL
                                    it_booking_supplement TYPE /dmo/if_flight_legacy=>tt_booking_supplement_in OPTIONAL
                          EXPORTING es_travel             TYPE /dmo/travel
                                    et_booking            TYPE /dmo/if_flight_legacy=>tt_booking
                                    et_booking_supplement TYPE /dmo/if_flight_legacy=>tt_booking_supplement
                                    et_messages           TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS update_travel IMPORTING is_travel              TYPE /dmo/if_flight_legacy=>ts_travel_in
                                    is_travelx             TYPE /dmo/if_flight_legacy=>ts_travel_inx
                                    it_booking             TYPE /dmo/if_flight_legacy=>tt_booking_in OPTIONAL
                                    it_bookingx            TYPE /dmo/if_flight_legacy=>tt_booking_inx OPTIONAL
                                    it_booking_supplement  TYPE /dmo/if_flight_legacy=>tt_booking_supplement_in OPTIONAL
                                    it_booking_supplementx TYPE /dmo/if_flight_legacy=>tt_booking_supplement_inx OPTIONAL
                          EXPORTING es_travel              TYPE /dmo/travel
                                    et_booking             TYPE /dmo/if_flight_legacy=>tt_booking
                                    et_booking_supplement  TYPE /dmo/if_flight_legacy=>tt_booking_supplement
                                    et_messages            TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS delete_travel IMPORTING iv_travel_id TYPE /dmo/travel_id
                          EXPORTING et_messages  TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS get_travel IMPORTING iv_travel_id          TYPE /dmo/travel_id
                       EXPORTING es_travel             TYPE /dmo/travel
                                 et_booking            TYPE /dmo/if_flight_legacy=>tt_booking
                                 et_booking_supplement TYPE /dmo/if_flight_legacy=>tt_booking_supplement
                                 et_messages           TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS save.
    METHODS initialize.
    METHODS convert_messages IMPORTING it_messages              TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                             EXPORTING et_messages              TYPE /dmo/if_flight_legacy=>tt_message.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO /dmo/cl_flight_legacy.

    CLASS-METHODS:
      "! Calculation of Price <br/>
      "!  <br/>
      "! Price will be calculated using distance multiplied and occupied seats.<br/>
      "! Depending on how many seats in percentage are occupied the formula <br/>
      "! 3/400·x² + 25<br/>
      "! will be applied.<br/>
      "!   0% seats occupied leads to 25% of distance as price.<br/>
      "!  75% seats occupied leads to 50% of distance as price.<br/>
      "! 100% seats occupied leads to 100% of distance as price.<br/>
      "! @parameter iv_seats_occupied_percent | occupied seats
      "! @parameter iv_flight_distance | flight distance in kilometer
      "! @parameter rv_price | calculated flight price
      calculate_flight_price
        IMPORTING
          iv_seats_occupied_percent TYPE /dmo/plane_seats_occupied
          iv_flight_distance        TYPE i
        RETURNING
          VALUE(rv_price)           TYPE /dmo/flight_price ##RELAX.

    METHODS lock_travel IMPORTING iv_lock TYPE abap_bool
                        RAISING   /dmo/cx_flight_legacy ##RELAX.

    METHODS _resolve_attribute IMPORTING iv_attrname      TYPE scx_attrname
                                         ix               TYPE REF TO /dmo/cx_flight_legacy
                               RETURNING VALUE(rv_symsgv) TYPE symsgv.
ENDCLASS.



CLASS /dmo/cl_flight_legacy IMPLEMENTATION.


  METHOD calculate_flight_price.
    DATA: lv_percentage_of_max_price TYPE i.
    lv_percentage_of_max_price = ( 3 * iv_seats_occupied_percent ** 2 DIV 400 ) + 25 ##OPERATOR[**].
    rv_price = lv_percentage_of_max_price * iv_flight_distance DIV 100.
  ENDMETHOD.


  METHOD convert_messages.
    CLEAR et_messages.
    DATA ls_message TYPE symsg.
    LOOP AT it_messages INTO DATA(lr_error) ##INTO_OK.
      ls_message-msgty = 'E'.
      ls_message-msgid = lr_error->t100key-msgid.
      ls_message-msgno = lr_error->t100key-msgno.
      IF lr_error IS INSTANCE OF /dmo/cx_flight_legacy.
        DATA(lx) = CAST /dmo/cx_flight_legacy( lr_error ).
        ls_message-msgv1 = _resolve_attribute( iv_attrname = lr_error->t100key-attr1  ix = lx ).
        ls_message-msgv2 = _resolve_attribute( iv_attrname = lr_error->t100key-attr2  ix = lx ).
        ls_message-msgv3 = _resolve_attribute( iv_attrname = lr_error->t100key-attr3  ix = lx ).
        ls_message-msgv4 = _resolve_attribute( iv_attrname = lr_error->t100key-attr4  ix = lx ).
      ENDIF.
      APPEND ls_message TO et_messages.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_instance.
    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD initialize.
    lcl_travel_buffer=>get_instance( )->initialize( ).
    lcl_booking_buffer=>get_instance( )->initialize( ).
    lcl_booking_supplement_buffer=>get_instance( )->initialize( ).
  ENDMETHOD.


  METHOD lock_travel.
    IF iv_lock = abap_true.
      CALL FUNCTION 'ENQUEUE_/DMO/ETRAVEL'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /dmo/cx_flight_legacy
          EXPORTING
            textid = /dmo/cx_flight_legacy=>travel_lock.
      ENDIF.
    ELSE.
      CALL FUNCTION 'DEQUEUE_/DMO/ETRAVEL'.
    ENDIF.
  ENDMETHOD.


  METHOD save.
    lcl_travel_buffer=>get_instance( )->save( ).
    lcl_booking_buffer=>get_instance( )->save( ).
    lcl_booking_supplement_buffer=>get_instance( )->save( ).
    initialize( ).
  ENDMETHOD.


  METHOD set_status_to_booked.
    lcl_travel_buffer=>get_instance( )->set_status_to_booked( EXPORTING iv_travel_id = iv_travel_id
                                                              IMPORTING et_messages  = et_messages ).
  ENDMETHOD.


  METHOD create_travel.
    CLEAR: es_travel, et_booking, et_booking_supplement, et_messages.

    " Travel
    lcl_travel_buffer=>get_instance( )->cud_prep( EXPORTING it_travel   = VALUE #( ( CORRESPONDING #( is_travel ) ) )
                                                            it_travelx  = VALUE #( ( travel_id = is_travel-travel_id  action_code = /dmo/if_flight_legacy=>action_code-create ) )
                                                  IMPORTING et_travel   = DATA(lt_travel)
                                                            et_messages = et_messages ).
    IF et_messages IS INITIAL.
      ASSERT lines( lt_travel ) = 1.
      es_travel = lt_travel[ 1 ].
    ENDIF.

    " Bookings
    IF et_messages IS INITIAL.
      DATA lt_booking  TYPE /dmo/if_flight_legacy=>tt_booking.
      DATA lt_bookingx TYPE /dmo/if_flight_legacy=>tt_bookingx.
      LOOP AT it_booking INTO DATA(ls_booking_in).
        DATA ls_booking TYPE /dmo/booking.
        ls_booking = CORRESPONDING #( ls_booking_in ).
        ls_booking-travel_id = es_travel-travel_id.
        INSERT ls_booking INTO TABLE lt_booking.
        INSERT VALUE #( travel_id = ls_booking-travel_id  booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-create ) INTO TABLE lt_bookingx.
      ENDLOOP.
      lcl_booking_buffer=>get_instance( )->cud_prep( EXPORTING it_booking  = lt_booking
                                                               it_bookingx = lt_bookingx
                                                     IMPORTING et_booking  = et_booking
                                                               et_messages = DATA(lt_messages) ).
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.

    " Booking Supplements
    IF et_messages IS INITIAL.
      DATA lt_booking_supplement  TYPE /dmo/if_flight_legacy=>tt_booking_supplement.
      DATA lt_booking_supplementx TYPE /dmo/if_flight_legacy=>tt_booking_supplementx.
      LOOP AT it_booking_supplement INTO DATA(ls_booking_supplement_in).
        DATA ls_booking_supplement TYPE /dmo/book_suppl.
        ls_booking_supplement = CORRESPONDING #( ls_booking_supplement_in ).
        ls_booking_supplement-travel_id = es_travel-travel_id.
        IF lcl_booking_buffer=>get_instance( )->check_booking_id( EXPORTING iv_travel_id = ls_booking_supplement-travel_id  iv_booking_id = ls_booking_supplement-booking_id CHANGING ct_messages = et_messages ) = abap_false.
          EXIT.
        ENDIF.
        INSERT ls_booking_supplement INTO TABLE lt_booking_supplement.
        INSERT VALUE #( travel_id             = ls_booking_supplement-travel_id
                        booking_id            = ls_booking_supplement-booking_id
                        booking_supplement_id = ls_booking_supplement-booking_supplement_id
                        action_code           = /dmo/if_flight_legacy=>action_code-create ) INTO TABLE lt_booking_supplementx.
      ENDLOOP.
      IF et_messages IS INITIAL.
        lcl_booking_supplement_buffer=>get_instance( )->cud_prep( EXPORTING it_booking_supplement  = lt_booking_supplement
                                                                            it_booking_supplementx = lt_booking_supplementx
                                                                  IMPORTING et_booking_supplement  = et_booking_supplement
                                                                            et_messages            = lt_messages ).
        APPEND LINES OF lt_messages TO et_messages.
      ENDIF.
    ENDIF.

    IF et_messages IS INITIAL.
      lcl_travel_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_copy( ).
    ELSE.
      CLEAR: es_travel, et_booking, et_booking_supplement.
      lcl_travel_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_disc( ).
    ENDIF.
  ENDMETHOD.


  METHOD update_travel.
    CLEAR es_travel.
    CLEAR et_booking.
    CLEAR et_booking_supplement.
    CLEAR et_messages.

    " Travel
    IF is_travel-travel_id IS INITIAL.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
      RETURN.
    ENDIF.
    DATA ls_travelx TYPE /dmo/if_flight_legacy=>ts_travelx.
    ls_travelx = CORRESPONDING #( is_travelx ).
    ls_travelx-action_code = /dmo/if_flight_legacy=>action_code-update.
    lcl_travel_buffer=>get_instance( )->cud_prep( EXPORTING it_travel   = VALUE #( ( CORRESPONDING #( is_travel ) ) )
                                                            it_travelx  = VALUE #( ( ls_travelx ) )
                                                  IMPORTING et_travel   = DATA(lt_travel)
                                                            et_messages = et_messages ).

    " Bookings
    IF et_messages IS INITIAL.
      " Ignore provided Travel ID of subnode tables
      DATA lt_booking  TYPE /dmo/if_flight_legacy=>tt_booking.
      DATA lt_bookingx TYPE /dmo/if_flight_legacy=>tt_bookingx.
      LOOP AT it_booking INTO DATA(ls_booking_in).
        DATA ls_booking TYPE /dmo/booking.
        ls_booking = CORRESPONDING #( ls_booking_in ).
        ls_booking-travel_id = is_travel-travel_id.
        INSERT ls_booking INTO TABLE lt_booking.
      ENDLOOP.
      LOOP AT it_bookingx INTO DATA(ls_booking_inx).
        DATA ls_bookingx TYPE /dmo/if_flight_legacy=>ts_bookingx.
        ls_bookingx = CORRESPONDING #( ls_booking_inx ).
        ls_bookingx-travel_id = is_travel-travel_id.
        INSERT ls_bookingx INTO TABLE lt_bookingx.
      ENDLOOP.
      lcl_booking_buffer=>get_instance( )->cud_prep( EXPORTING it_booking  = lt_booking
                                                               it_bookingx = lt_bookingx
                                                     IMPORTING et_booking  = et_booking
                                                               et_messages = DATA(lt_messages) ).
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.

    " Booking Supplements
    IF et_messages IS INITIAL.
      " Ignore provided Travel ID of subnode tables
      DATA lt_booking_supplement  TYPE /dmo/if_flight_legacy=>tt_booking_supplement.
      DATA lt_booking_supplementx TYPE /dmo/if_flight_legacy=>tt_booking_supplementx.
      LOOP AT it_booking_supplement INTO DATA(ls_booking_supplement_in).
        DATA ls_booking_supplement TYPE /dmo/book_suppl.
        ls_booking_supplement = CORRESPONDING #( ls_booking_supplement_in ).
        ls_booking_supplement-travel_id = is_travel-travel_id.
        IF lcl_booking_buffer=>get_instance( )->check_booking_id( EXPORTING iv_travel_id  = ls_booking_supplement-travel_id
                                                                            iv_booking_id = ls_booking_supplement-booking_id
                                                                  CHANGING  ct_messages   = et_messages ) = abap_false.
          EXIT.
        ENDIF.
        INSERT ls_booking_supplement INTO TABLE lt_booking_supplement.
      ENDLOOP.
      IF et_messages IS INITIAL.
        LOOP AT it_booking_supplementx INTO DATA(ls_booking_supplement_inx).
          DATA ls_booking_supplementx TYPE /dmo/if_flight_legacy=>ts_booking_supplementx.
          ls_booking_supplementx = CORRESPONDING #( ls_booking_supplement_inx ).
          ls_booking_supplementx-travel_id = is_travel-travel_id.
          INSERT ls_booking_supplementx INTO TABLE lt_booking_supplementx.
        ENDLOOP.
        lcl_booking_supplement_buffer=>get_instance( )->cud_prep( EXPORTING it_booking_supplement  = lt_booking_supplement
                                                                            it_booking_supplementx = lt_booking_supplementx
                                                                  IMPORTING et_booking_supplement  = et_booking_supplement
                                                                            et_messages            = lt_messages ).
        APPEND LINES OF lt_messages TO et_messages.
      ENDIF.
    ENDIF.

    IF et_messages IS INITIAL.
      lcl_travel_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_copy( ).
      LOOP AT lt_bookingx ASSIGNING FIELD-SYMBOL(<s_bookingx>) WHERE action_code = CONV /dmo/action_code( /dmo/if_flight_legacy=>action_code-delete ).
        lcl_booking_supplement_buffer=>get_instance( )->delete_booking_supplements( VALUE #( ( travel_id = <s_bookingx>-travel_id  booking_id = <s_bookingx>-booking_id ) ) ).
      ENDLOOP.
      ASSERT lines( lt_travel ) = 1.
      es_travel = lt_travel[ 1 ].
    ELSE.
      lcl_travel_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_disc( ).
    ENDIF.
  ENDMETHOD.


  METHOD delete_travel.
    CLEAR et_messages.
    lcl_travel_buffer=>get_instance( )->cud_prep( EXPORTING it_travel   = VALUE #( ( travel_id = iv_travel_id ) )
                                                            it_travelx  = VALUE #( ( travel_id = iv_travel_id  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                                                  IMPORTING et_messages = et_messages ).
    IF et_messages IS INITIAL.
      lcl_travel_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_supplement_buffer=>get_instance( )->delete_booking_supplements( VALUE #( ( travel_id = iv_travel_id ) ) ).
      lcl_booking_buffer=>get_instance( )->delete_bookings( VALUE #( ( travel_id = iv_travel_id ) ) ).
    ELSE.
      lcl_travel_buffer=>get_instance( )->cud_disc( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_travel.
    CLEAR: es_travel, et_booking, et_booking_supplement, et_messages.

    IF iv_travel_id IS INITIAL.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
      RETURN.
    ENDIF.

    lcl_travel_buffer=>get_instance( )->get( EXPORTING it_travel = VALUE #( ( travel_id = iv_travel_id ) ) IMPORTING et_travel = DATA(lt_travel) ).
    IF lt_travel IS INITIAL.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = iv_travel_id ) TO et_messages.
      RETURN.
    ENDIF.
    ASSERT lines( lt_travel ) = 1.
    es_travel = lt_travel[ 1 ].

    lcl_booking_buffer=>get_instance( )->get( EXPORTING it_booking = VALUE #( ( travel_id = iv_travel_id ) ) IMPORTING et_booking = et_booking ).

    lcl_booking_supplement_buffer=>get_instance( )->get( EXPORTING it_booking_supplement = CORRESPONDING #( et_booking MAPPING travel_id = travel_id  booking_id = booking_id EXCEPT * )
                                                         IMPORTING et_booking_supplement = et_booking_supplement ).
  ENDMETHOD.


  METHOD _resolve_attribute.
    CLEAR rv_symsgv.
    CASE iv_attrname.
      WHEN ''.                         rv_symsgv = ''.
      WHEN 'MV_TRAVEL_ID'.             rv_symsgv = ix->mv_travel_id.
      WHEN 'MV_BOOKING_ID'.            rv_symsgv = ix->mv_booking_id.
      WHEN 'MV_BOOKING_SUPPLEMENT_ID'. rv_symsgv = ix->mv_booking_supplement_id.
      WHEN 'MV_AGENCY_ID'.             rv_symsgv = ix->mv_agency_id.
      WHEN 'MV_CUSTOMER_ID'.           rv_symsgv = ix->mv_customer_id.
      WHEN 'MV_CARRIER_ID'.            rv_symsgv = ix->mv_carrier_id.
      WHEN 'MV_CONNECTION_ID'.         rv_symsgv = ix->mv_connection_id.
      WHEN 'MV_SUPPLEMENT_ID'.         rv_symsgv = ix->mv_supplement_id.
      WHEN 'MV_BEGIN_DATE'.            rv_symsgv = ix->mv_begin_date.
      WHEN 'MV_END_DATE'.              rv_symsgv = ix->mv_end_date.
      WHEN 'MV_BOOKING_DATE'.          rv_symsgv = ix->mv_booking_date.
      WHEN 'MV_FLIGHT_DATE'.           rv_symsgv = ix->mv_flight_date.
      when 'MV_STATUS'.                rv_symsgv = ix->mv_status.
      WHEN OTHERS.  ASSERT 1 = 2.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
