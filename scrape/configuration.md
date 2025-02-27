# Configuration

## Clientbound

### Cookie Request (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Key                  | String   | Cookie identifier    | 0x00         |
+----------------------+----------+----------------------+--------------+

### Clientbound Plugin Message (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Channel              | String   | Plugin channel name  | 0x01         |
| Data                 | Byte[]   | Plugin data          |              |
+----------------------+----------+----------------------+--------------+

### Disconnect (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Reason               | String   | Disconnect message   | 0x02         |
+----------------------+----------+----------------------+--------------+

### Finish Configuration
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x03         |
+----------------------+----------+----------------------+--------------+

### Clientbound Keep Alive (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Keep alive ID        | 0x04         |
+----------------------+----------+----------------------+--------------+

### Ping (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Ping ID              | 0x05         |
+----------------------+----------+----------------------+--------------+

### Reset Chat
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x06         |
+----------------------+----------+----------------------+--------------+

### Registry Data
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Data                 | Byte[]   | Registry data        | 0x07         |
+----------------------+----------+----------------------+--------------+

### Remove Resource Pack (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x08         |
+----------------------+----------+----------------------+--------------+

### Add Resource Pack (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| URL                  | String   | Resource pack URL    | 0x09         |
| Hash                 | String   | Resource pack hash   |              |
+----------------------+----------+----------------------+--------------+

### Store Cookie (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Key                  | String   | Cookie identifier    | 0x0A         |
| Value                | String   | Cookie value         |              |
+----------------------+----------+----------------------+--------------+

### Transfer (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Server Address       | String   | Server address       | 0x0B         |
| Server Port          | UShort   | Server port          |              |
+----------------------+----------+----------------------+--------------+

### Feature Flags
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Flags                | Byte[]   | Feature flags        | 0x0C         |
+----------------------+----------+----------------------+--------------+

### Update Tags (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Tags                 | Byte[]   | Tags data            | 0x0D         |
+----------------------+----------+----------------------+--------------+

### Clientbound Known Packs
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Packs                | Byte[]   | Known packs data     | 0x0E         |
+----------------------+----------+----------------------+--------------+

### Custom Report Details (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Details              | Byte[]   | Report details       | 0x0F         |
+----------------------+----------+----------------------+--------------+

### Server Links (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Links                | Byte[]   | Server links data    | 0x10         |
+----------------------+----------+----------------------+--------------+

## Serverbound

### Client Information (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Info                 | Byte[]   | Client information   | 0x00         |
+----------------------+----------+----------------------+--------------+

### Cookie Response (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Key                  | String   | Cookie identifier    | 0x01         |
| Value                | String   | Cookie value         |              |
+----------------------+----------+----------------------+--------------+

### Serverbound Plugin Message (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Channel              | String   | Plugin channel name  | 0x02         |
| Data                 | Byte[]   | Plugin data          |              |
+----------------------+----------+----------------------+--------------+

### Acknowledge Finish Configuration
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x03         |
+----------------------+----------+----------------------+--------------+

### Serverbound Keep Alive (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Keep alive ID        | 0x04         |
+----------------------+----------+----------------------+--------------+

### Pong (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Pong ID              | 0x05         |
+----------------------+----------+----------------------+--------------+

### Resource Pack Response (configuration)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Status               | VarInt   | Response status      | 0x06         |
+----------------------+----------+----------------------+--------------+

### Serverbound Known Packs
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Packs                | Byte[]   | Known packs data     | 0x07         |
+----------------------+----------+----------------------+--------------+
