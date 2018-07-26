INTERFACE /dmo/if_flight_legacy
  PUBLIC.

  TYPES BEGIN OF ts_travel_key.
  TYPES travel_id TYPE /dmo/travel_id.
  TYPES END OF ts_travel_key.
  TYPES tt_travel_key TYPE SORTED TABLE OF ts_travel_key WITH UNIQUE KEY travel_id.

  TYPES BEGIN OF ts_booking_key.
  INCLUDE TYPE ts_travel_key.
  TYPES booking_id TYPE /dmo/booking_id.
  TYPES END OF ts_booking_key.
  TYPES tt_booking_key TYPE SORTED TABLE OF ts_booking_key WITH UNIQUE KEY travel_id  booking_id.

  TYPES BEGIN OF ts_booking_supplement_key.
  INCLUDE TYPE ts_booking_key.
  TYPES booking_supplement_id TYPE /dmo/booking_supplement_id.
  TYPES END OF ts_booking_supplement_key.
  TYPES tt_booking_supplement_key TYPE SORTED TABLE OF ts_booking_supplement_key WITH UNIQUE KEY travel_id  booking_id  booking_supplement_id.

  TYPES: tt_travel             TYPE SORTED TABLE OF /dmo/travel     WITH UNIQUE KEY travel_id.
  TYPES: tt_booking            TYPE SORTED TABLE OF /dmo/booking    WITH UNIQUE KEY travel_id  booking_id.
  TYPES: tt_booking_supplement TYPE SORTED TABLE OF /dmo/book_suppl WITH UNIQUE KEY travel_id  booking_id  booking_supplement_id.

***********************************************************************************************************************************
* IMPORTANT: When you add or remove fields from /DMO/TRAVEL, /DMO/BOOKING, /DMO/BOOK_SUPPL you need to change the following types *
***********************************************************************************************************************************
  TYPES: BEGIN OF ts_travel_intx,
           agency_id     TYPE abap_bool,
           customer_id   TYPE abap_bool,
           begin_date    TYPE abap_bool,
           end_date      TYPE abap_bool,
           booking_fee   TYPE abap_bool,
           total_price   TYPE abap_bool,
           currency_code TYPE abap_bool,
           description   TYPE abap_bool,
           status        TYPE abap_bool,
         END OF ts_travel_intx.
  TYPES: BEGIN OF ts_booking_intx,
           booking_date  TYPE abap_bool,
           customer_id   TYPE abap_bool,
           carrier_id    TYPE abap_bool,
           connection_id TYPE abap_bool,
           flight_date   TYPE abap_bool,
           flight_price  TYPE abap_bool,
           currency_code TYPE abap_bool,
         END OF ts_booking_intx.
  TYPES: BEGIN OF ts_booking_supplement_intx,
           supplement_id TYPE abap_bool,
           price         TYPE abap_bool,
           currency_code TYPE abap_bool,
         END OF ts_booking_supplement_intx.

" Internally we use the full X-structures: With complete key and action code
  TYPES BEGIN OF ts_travelx.
  INCLUDE TYPE ts_travel_key.
  TYPES action_code TYPE /dmo/action_code.
  INCLUDE TYPE ts_travel_intx.
  TYPES END OF ts_travelx.
  TYPES: tt_travelx TYPE SORTED TABLE OF ts_travelx WITH UNIQUE KEY travel_id.
*
  TYPES BEGIN OF ts_bookingx.
  INCLUDE TYPE ts_booking_key.
  TYPES action_code TYPE /dmo/action_code.
  INCLUDE TYPE ts_booking_intx.
  TYPES END OF ts_bookingx.
  TYPES: tt_bookingx TYPE SORTED TABLE OF ts_bookingx WITH UNIQUE KEY travel_id  booking_id.
*
  TYPES BEGIN OF ts_booking_supplementx.
  INCLUDE TYPE ts_booking_supplement_key.
  TYPES action_code TYPE /dmo/action_code.
  INCLUDE TYPE ts_booking_supplement_intx.
  TYPES END OF ts_booking_supplementx.
  TYPES: tt_booking_supplementx TYPE SORTED TABLE OF ts_booking_supplementx WITH UNIQUE KEY travel_id  booking_id  booking_supplement_id.

  TYPES: BEGIN OF ENUM action_code_enum STRUCTURE action_code BASE TYPE /dmo/action_code,
           initial VALUE IS INITIAL,
           create  VALUE 'C',
           update  VALUE 'U',
           delete  VALUE 'D',
         END OF ENUM action_code_enum STRUCTURE action_code.

  " The caller of the BAPI like function modules shall not provide the administrative fields
  TYPES BEGIN OF ts_travel_in.
  INCLUDE TYPE ts_travel_key.
  INCLUDE TYPE /dmo/travel_data.
  TYPES END OF ts_travel_in.

  " The BAPI like function modules always refer to a single travel.
  " Therefore the Travel ID is not required in the subnode tables
  TYPES BEGIN OF ts_booking_in.
  TYPES booking_id TYPE /dmo/booking_id.
  INCLUDE TYPE /dmo/booking_data.
  TYPES END OF ts_booking_in.
  TYPES tt_booking_in TYPE SORTED TABLE OF ts_booking_in WITH UNIQUE KEY booking_id.

  " The BAPI like function modules always refer to a single travel.
  " Therefore the Travel ID is not required in the subnode tables
  TYPES BEGIN OF ts_booking_supplement_in.
  TYPES booking_id TYPE /dmo/booking_id.
  TYPES booking_supplement_id TYPE /dmo/booking_supplement_id.
  INCLUDE TYPE /dmo/book_suppl_data.
  TYPES END OF ts_booking_supplement_in.
  TYPES tt_booking_supplement_in TYPE SORTED TABLE OF ts_booking_supplement_in WITH UNIQUE KEY booking_id  booking_supplement_id.

  " The caller of the BAPI like function modules shall not provide the administrative fields
  " Furthermore the action Code is not required for the root (because it is already determined by the function module name)
  TYPES BEGIN OF ts_travel_inx.
  INCLUDE TYPE ts_travel_key.
  INCLUDE TYPE ts_travel_intx.
  TYPES END OF ts_travel_inx.

  " The BAPI like function modules always refer to a single travel.
  " Therefore the Travel ID is not required in the subnode tables
  TYPES BEGIN OF ts_booking_inx.
  TYPES booking_id TYPE /dmo/booking_id.
  TYPES action_code TYPE /dmo/action_code.
  INCLUDE TYPE ts_booking_intx.
  TYPES END OF ts_booking_inx.
  TYPES tt_booking_inx TYPE SORTED TABLE OF ts_booking_inx WITH UNIQUE KEY booking_id.

  " The BAPI like function modules always refer to a single travel.
  " Therefore the Travel ID is not required in the subnode tables
  TYPES BEGIN OF ts_booking_supplement_inx.
  TYPES           booking_id            TYPE /dmo/booking_id.
  TYPES           booking_supplement_id TYPE /dmo/booking_supplement_id.
  TYPES           action_code           TYPE /dmo/action_code.
  INCLUDE TYPE ts_booking_supplement_intx.
  TYPES END OF ts_booking_supplement_inx.
  TYPES tt_booking_supplement_inx TYPE SORTED TABLE OF ts_booking_supplement_inx WITH UNIQUE KEY booking_id  booking_supplement_id.

  TYPES tt_message TYPE STANDARD TABLE OF symsg.

  " Internally we use a table of exceptions.
  " We have only error messages.
  " Currently we do not communicate Warnings or Success Messages.
  TYPES tt_if_t100_message TYPE STANDARD TABLE OF REF TO if_t100_message WITH EMPTY KEY.

  TYPES: BEGIN OF ENUM travel_status_enum STRUCTURE travel_status BASE TYPE /dmo/travel_status,
           initial   VALUE IS INITIAL,
           new       VALUE 'N',
           planned   VALUE 'P',
           booked    VALUE 'B',
           cancelled VALUE 'X',
         END OF ENUM travel_status_enum STRUCTURE travel_status.

  TYPES tt_flight TYPE STANDARD TABLE OF /dmo/flight WITH KEY client carrier_id connection_id flight_date.
ENDINTERFACE.
