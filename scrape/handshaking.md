# Handshaking

## Clientbound
None

## Serverbound

### Handshake
+------------------+------------+-------------+----------------+
| Field Name       | Field Type | Notes       | Protocol ID    |
+------------------+------------+-------------+----------------+
| Protocol Version | VarInt     | Current 767 | 0x00           |
| Server Address   | String     | Max 255     |                |
| Server Port      | UShort     | Usually     |                |
|                  |            | 25565       |                |
| Next State       | VarInt     | Status=1,   |                |
|                  |            | Login=2,    |                |
|                  |            | Transfer=3  |                |
+------------------+------------+-------------+----------------+

### Legacy Server List Ping
+-------------+------------+-------------+----------------+
| Field Name  | Field Type | Notes       | Protocol ID    |
+-------------+------------+-------------+----------------+
| Payload     | UByte      | Always 0x01 | 0xFE           |
+-------------+------------+-------------+----------------+