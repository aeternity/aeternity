* Provides a way to setup the API webserver to skip reading the http 
  request body after an amount of bytes. When the length is too large
  the webserver will close the connection. This is initially meant for 
  the Mdw but Nodes with lower RAM spec would benefit too.
  On sys.config, it's an `aehttp`
  config with the property:
    ```
    {protocol_options, [
        {max_skip_body_length, 1000000}
        ]}
    ```
  The minimum recommended value is 400000 to fully read a POST with the max
  amount of bytes/gas that AE protocol allows in a micro block.
  