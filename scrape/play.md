# Play

## Clientbound

### Bundle Delimiter
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x00         |
+----------------------+----------+----------------------+--------------+

### Spawn Entity
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x01         |
| UUID                 | UUID     | Entity UUID          |              |
| Type                 | VarInt   | Entity type          |              |
| X                    | Double   | X coordinate         |              |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Pitch                | Angle    | Pitch                |              |
| Yaw                  | Angle    | Yaw                  |              |
| Data                 | Int      | Entity data          |              |
| Velocity X           | Short    | X velocity           |              |
| Velocity Y           | Short    | Y velocity           |              |
| Velocity Z           | Short    | Z velocity           |              |
+----------------------+----------+----------------------+--------------+

### Spawn Experience Orb
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x02         |
| X                    | Double   | X coordinate         |              |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Count                | Short    | Experience count     |              |
+----------------------+----------+----------------------+--------------+

### Entity Animation
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x03         |
| Animation            | UByte    | Animation type       |              |
+----------------------+----------+----------------------+--------------+

### Award Statistics
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Statistics           | VarInt[] | List of statistics   | 0x04         |
+----------------------+----------+----------------------+--------------+

### Acknowledge Block Change
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Sequence             | VarInt   | Block change sequence| 0x05         |
+----------------------+----------+----------------------+--------------+

### Set Block Destroy Stage
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x06         |
| Location             | Position | Block position       |              |
| Destroy Stage        | Byte     | Destroy stage        |              |
+----------------------+----------+----------------------+--------------+

### Block Entity Data
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Block position       | 0x07         |
| Action               | UByte    | Action type          |              |
| NBT Data             | NBT      | Block entity data    |              |
+----------------------+----------+----------------------+--------------+

### Block Action
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Block position       | 0x08         |
| Action ID            | UByte    | Action ID            |              |
| Action Param         | UByte    | Action parameter     |              |
| Block Type           | VarInt   | Block type           |              |
+----------------------+----------+----------------------+--------------+

### Block Update
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Block position       | 0x09         |
| Block ID             | VarInt   | Block ID             |              |
+----------------------+----------+----------------------+--------------+

### Boss Bar
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| UUID                 | UUID     | Boss bar UUID        | 0x0A         |
| Action               | VarInt   | Action type          |              |
| Title                | String   | Boss bar title       |              |
| Health               | Float    | Boss bar health      |              |
| Color                | VarInt   | Boss bar color       |              |
| Division             | VarInt   | Boss bar division    |              |
| Flags                | UByte    | Boss bar flags       |              |
+----------------------+----------+----------------------+--------------+

### Change Difficulty
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Difficulty           | UByte    | Difficulty level     | 0x0B         |
+----------------------+----------+----------------------+--------------+

### Chunk Batch Finished
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x0C         |
+----------------------+----------+----------------------+--------------+

### Chunk Batch Start
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x0D         |
+----------------------+----------+----------------------+--------------+

### Chunk Biomes
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Chunk X              | Int      | Chunk X coordinate   | 0x0E         |
| Chunk Z              | Int      | Chunk Z coordinate   |              |
| Biomes               | Byte[]   | Biome data           |              |
+----------------------+----------+----------------------+--------------+

### Clear Titles
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x0F         |
+----------------------+----------+----------------------+--------------+

### Command Suggestions Response
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | VarInt   | Suggestions ID       | 0x10         |
| Suggestions          | String[] | List of suggestions  |              |
+----------------------+----------+----------------------+--------------+

### Commands
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Commands             | Byte[]   | Command data         | 0x11         |
+----------------------+----------+----------------------+--------------+

### Close Container
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x12         |
+----------------------+----------+----------------------+--------------+

### Set Container Content
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x13         |
| Slots                | Slot[]   | List of slots        |              |
+----------------------+----------+----------------------+--------------+

### Set Container Property
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x14         |
| Property             | Short    | Property ID          |              |
| Value                | Short    | Property value       |              |
+----------------------+----------+----------------------+--------------+

### Set Container Slot
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x15         |
| Slot                 | Short    | Slot ID              |              |
| Slot Data            | Slot     | Slot data            |              |
+----------------------+----------+----------------------+--------------+

### Cookie Request (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Key                  | String   | Cookie identifier    | 0x16         |
+----------------------+----------+----------------------+--------------+

### Set Cooldown
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Item ID              | VarInt   | Item ID              | 0x17         |
| Cooldown             | VarInt   | Cooldown duration    |              |
+----------------------+----------+----------------------+--------------+

### Chat Suggestions
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | VarInt   | Suggestions ID       | 0x18         |
| Suggestions          | String[] | List of suggestions  |              |
+----------------------+----------+----------------------+--------------+

### Clientbound Plugin Message (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Channel              | String   | Plugin channel name  | 0x19         |
| Data                 | Byte[]   | Plugin data          |              |
+----------------------+----------+----------------------+--------------+

### Damage Event
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x1A         |
| Source ID            | VarInt   | Damage source ID     |              |
| Type                 | VarInt   | Damage type          |              |
| Amount               | Float    | Damage amount        |              |
+----------------------+----------+----------------------+--------------+

### Debug Sample
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Data                 | Byte[]   | Debug sample data    | 0x1B         |
+----------------------+----------+----------------------+--------------+

### Delete Message
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Message ID           | UUID     | Message UUID         | 0x1C         |
+----------------------+----------+----------------------+--------------+

### Disconnect (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Reason               | String   | Disconnect message   | 0x1D         |
+----------------------+----------+----------------------+--------------+

### Disguised Chat Message
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Sender               | UUID     | Sender UUID          | 0x1E         |
| Message              | String   | Chat message         |              |
+----------------------+----------+----------------------+--------------+

### Entity Event
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x1F         |
| Event                | UByte    | Event type           |              |
+----------------------+----------+----------------------+--------------+

### Teleport Entity
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x20         |
| X                    | Double   | X coordinate         |              |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Yaw                  | Angle    | Yaw                  |              |
| Pitch                | Angle    | Pitch                |              |
| On Ground            | Boolean  | On ground flag       |              |
+----------------------+----------+----------------------+--------------+

### Explosion
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| X                    | Float    | X coordinate         | 0x21         |
| Y                    | Float    | Y coordinate         |              |
| Z                    | Float    | Z coordinate         |              |
| Radius               | Float    | Explosion radius     |              |
| Affected Blocks      | Position[]| List of affected blocks|           |
| Player Motion X      | Float    | Player motion X      |              |
| Player Motion Y      | Float    | Player motion Y      |              |
| Player Motion Z      | Float    | Player motion Z      |              |
+----------------------+----------+----------------------+--------------+

### Unload Chunk
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Chunk X              | Int      | Chunk X coordinate   | 0x22         |
| Chunk Z              | Int      | Chunk Z coordinate   |              |
+----------------------+----------+----------------------+--------------+

### Game Event
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Event                | VarInt   | Event type           | 0x23         |
| Data                 | Float    | Event data           |              |
+----------------------+----------+----------------------+--------------+

### Open Horse Screen
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x24         |
| Slot Count           | VarInt   | Number of slots      |              |
| Entity ID            | Int      | Horse entity ID      |              |
+----------------------+----------+----------------------+--------------+

### Hurt Animation
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x25         |
| Damage Direction     | Float    | Damage direction     |              |
+----------------------+----------+----------------------+--------------+

### Initialize World Border
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| X                    | Double   | X coordinate         | 0x26         |
| Z                    | Double   | Z coordinate         |              |
| Old Radius           | Double   | Old radius           |              |
| New Radius           | Double   | New radius           |              |
| Speed                | VarLong  | Speed                |              |
| Portal Teleport Boundary | VarInt | Portal teleport boundary |        |
| Warning Time         | VarInt   | Warning time         |              |
| Warning Blocks       | VarInt   | Warning blocks       |              |
+----------------------+----------+----------------------+--------------+

### Clientbound Keep Alive (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Keep alive ID        | 0x27         |
+----------------------+----------+----------------------+--------------+

### Chunk Data and Update Light
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Chunk X              | Int      | Chunk X coordinate   | 0x28         |
| Chunk Z              | Int      | Chunk Z coordinate   |              |
| Ground-Up Continuous | Boolean  | Ground-up continuous |              |
| Primary Bit Mask     | VarInt   | Primary bit mask     |              |
| Heightmaps           | NBT      | Heightmaps           |              |
| Biomes               | Byte[]   | Biome data           |              |
| Data                 | Byte[]   | Chunk data           |              |
| Block Entities       | NBT[]    | Block entities       |              |
| Sky Light Mask       | VarInt   | Sky light mask       |              |
| Block Light Mask     | VarInt   | Block light mask     |              |
| Empty Sky Light Mask | VarInt   | Empty sky light mask |              |
| Empty Block Light Mask | VarInt | Empty block light mask |            |
| Sky Light Arrays     | Byte[][] | Sky light arrays     |              |
| Block Light Arrays   | Byte[][] | Block light arrays   |              |
+----------------------+----------+----------------------+--------------+

### World Event
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Event ID             | Int      | Event ID             | 0x29         |
| Location             | Position | Event location       |              |
| Data                 | Int      | Event data           |              |
| Global               | Boolean  | Global flag          |              |
+----------------------+----------+----------------------+--------------+

### Particle
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Particle ID          | Int      | Particle ID          | 0x2A         |
| Long Distance        | Boolean  | Long distance flag   |              |
| X                    | Double   | X coordinate         |              |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Offset X             | Float    | X offset             |              |
| Offset Y             | Float    | Y offset             |              |
| Offset Z             | Float    | Z offset             |              |
| Particle Data        | Float    | Particle data        |              |
| Particle Count       | Int      | Particle count       |              |
| Data                 | VarInt[] | Particle data array  |              |
+----------------------+----------+----------------------+--------------+

### Update Light
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Chunk X              | Int      | Chunk X coordinate   | 0x2B         |
| Chunk Z              | Int      | Chunk Z coordinate   |              |
| Sky Light Mask       | VarInt   | Sky light mask       |              |
| Block Light Mask     | VarInt   | Block light mask     |              |
| Empty Sky Light Mask | VarInt   | Empty sky light mask |              |
| Empty Block Light Mask | VarInt | Empty block light mask |            |
| Sky Light Arrays     | Byte[][] | Sky light arrays     |              |
| Block Light Arrays   | Byte[][] | Block light arrays   |              |
+----------------------+----------+----------------------+--------------+

### Login (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | Int      | Player entity ID     | 0x2C         |
| Is Hardcore          | Boolean  | Hardcore flag        |              |
| Game Mode            | UByte    | Game mode            |              |
| Previous Game Mode   | Byte     | Previous game mode   |              |
| World Names          | String[] | List of world names  |              |
| Dimension Codec      | NBT      | Dimension codec      |              |
| Dimension            | NBT      | Dimension            |              |
| World Name           | String   | World name           |              |
| Hashed Seed          | Long     | Hashed seed          |              |
| Max Players          | VarInt   | Max players          |              |
| View Distance        | VarInt   | View distance        |              |
| Simulation Distance  | VarInt   | Simulation distance  |              |
| Reduced Debug Info   | Boolean  | Reduced debug info   |              |
| Enable Respawn Screen| Boolean  | Enable respawn screen|              |
| Is Debug             | Boolean  | Debug world flag     |              |
| Is Flat              | Boolean  | Flat world flag      |              |
+----------------------+----------+----------------------+--------------+

### Map Data
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Map ID               | VarInt   | Map ID               | 0x2D         |
| Scale                | Byte     | Map scale            |              |
| Tracking Position    | Boolean  | Tracking position    |              |
| Locked               | Boolean  | Locked flag          |              |
| Icons                | Map Icon[]| List of map icons   |              |
| Columns              | Byte     | Number of columns    |              |
| Rows                 | Byte     | Number of rows       |              |
| X                    | Byte     | X coordinate         |              |
| Y                    | Byte     | Y coordinate         |              |
| Data                 | Byte[]   | Map data             |              |
+----------------------+----------+----------------------+--------------+

### Merchant Offers
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x2E         |
| Offers               | Merchant Offer[]| List of offers|              |
| Villager Level       | VarInt   | Villager level       |              |
| Experience           | VarInt   | Villager experience  |              |
| Is Regular Villager  | Boolean  | Regular villager flag|              |
| Can Restock          | Boolean  | Can restock flag     |              |
+----------------------+----------+----------------------+--------------+

### Update Entity Position
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x2F         |
| Delta X              | Short    | X position delta     |              |
| Delta Y              | Short    | Y position delta     |              |
| Delta Z              | Short    | Z position delta     |              |
| On Ground            | Boolean  | On ground flag       |              |
+----------------------+----------+----------------------+--------------+

### Update Entity Position and Rotation
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x30         |
| Delta X              | Short    | X position delta     |              |
| Delta Y              | Short    | Y position delta     |              |
| Delta Z              | Short    | Z position delta     |              |
| Yaw                  | Angle    | Yaw                  |              |
| Pitch                | Angle    | Pitch                |              |
| On Ground            | Boolean  | On ground flag       |              |
+----------------------+----------+----------------------+--------------+

### Move Minecart Along Track
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x31         |
| Location             | Position | Track position       |              |
+----------------------+----------+----------------------+--------------+

### Update Entity Rotation
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x32         |
| Yaw                  | Angle    | Yaw                  |              |
| Pitch                | Angle    | Pitch                |              |
| On Ground            | Boolean  | On ground flag       |              |
+----------------------+----------+----------------------+--------------+

### Move Vehicle
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| X                    | Double   | X coordinate         | 0x33         |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Yaw                  | Float    | Yaw                  |              |
| Pitch                | Float    | Pitch                |              |
+----------------------+----------+----------------------+--------------+

### Open Book
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Hand                 | VarInt   | Hand (0: main, 1: off)| 0x34        |
+----------------------+----------+----------------------+--------------+

### Open Screen
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | VarInt   | Window ID            | 0x35         |
| Type                 | String   | Screen type          |              |
| Title                | String   | Screen title         |              |
+----------------------+----------+----------------------+--------------+

### Open Sign Editor
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Sign position        | 0x36         |
+----------------------+----------+----------------------+--------------+

### Ping (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Ping ID              | 0x37         |
+----------------------+----------+----------------------+--------------+

### Ping Response (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Ping ID              | 0x38         |
+----------------------+----------+----------------------+--------------+

### Place Ghost Recipe
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x39         |
| Recipe               | String   | Recipe ID            |              |
+----------------------+----------+----------------------+--------------+

### Player Abilities (clientbound)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Flags                | Byte     | Ability flags        | 0x3A         |
| Flying Speed         | Float    | Flying speed         |              |
| Walking Speed        | Float    | Walking speed        |              |
+----------------------+----------+----------------------+--------------+

### Player Chat Message
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Sender               | UUID     | Sender UUID          | 0x3B         |
| Message              | String   | Chat message         |              |
| Timestamp            | Long     | Message timestamp    |              |
| Salt                 | Long     | Message salt         |              |
| Signature            | Byte[]   | Message signature    |              |
| Signed Preview       | Boolean  | Signed preview flag  |              |
| Last Seen Messages   | UUID[]   | List of last seen messages |        |
+----------------------+----------+----------------------+--------------+

### End Combat
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Duration             | VarInt   | Combat duration      | 0x3C         |
| Entity ID            | Int      | Entity ID            |              |
+----------------------+----------+----------------------+--------------+

### Enter Combat
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x3D         |
+----------------------+----------+----------------------+--------------+

### Combat Death
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Player ID            | VarInt   | Player ID            | 0x3E         |
| Entity ID            | Int      | Entity ID            |              |
| Message              | String   | Death message        |              |
+----------------------+----------+----------------------+--------------+

### Player Info Remove
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Players              | UUID[]   | List of player UUIDs | 0x3F         |
+----------------------+----------+----------------------+--------------+

### Player Info Update
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Action               | VarInt   | Update action        | 0x40         |
| Data                 | Byte[]   | Update data          |              |
+----------------------+----------+----------------------+--------------+

### Look At
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Target               | VarInt   | Look at target       | 0x41         |
| X                    | Double   | X coordinate         |              |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Is Entity            | Boolean  | Is entity flag       |              |
+----------------------+----------+----------------------+--------------+

### Synchronize Player Position
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| X                    | Double   | X coordinate         | 0x42         |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Yaw                  | Float    | Yaw                  |              |
| Pitch                | Float    | Pitch                |              |
| Flags                | Byte     | Position flags       |              |
| Teleport ID          | VarInt   | Teleport ID          |              |
+----------------------+----------+----------------------+--------------+

### Player Rotation
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Yaw                  | Float    | Yaw                  | 0x43         |
| Pitch                | Float    | Pitch                |              |
| On Ground            | Boolean  | On ground flag       |              |
+----------------------+----------+----------------------+--------------+

### Recipe Book Add
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Recipe               | String   | Recipe ID            | 0x44         |
+----------------------+----------+----------------------+--------------+

### Recipe Book Remove
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Recipe               | String   | Recipe ID            | 0x45         |
+----------------------+----------+----------------------+--------------+

### Recipe Book Settings
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Book ID              | VarInt   | Recipe book ID       | 0x46         |
| Book Open            | Boolean  | Book open flag       |              |
| Filter Active        | Boolean  | Filter active flag   |              |
+----------------------+----------+----------------------+--------------+

### Remove Entities
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity IDs           | VarInt[] | List of entity IDs   | 0x47         |
+----------------------+----------+----------------------+--------------+

### Remove Entity Effect
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x48         |
| Effect ID            | Byte     | Effect ID            |              |
+----------------------+----------+----------------------+--------------+

### Reset Score
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Objective Name       | String   | Objective name       | 0x49         |
| Player Name          | String   | Player name          |              |
| Action               | Byte     | Action type          |              |
+----------------------+----------+----------------------+--------------+

### Remove Resource Pack (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x4A         |
+----------------------+----------+----------------------+--------------+

### Add Resource Pack (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| URL                  | String   | Resource pack URL    | 0x4B         |
| Hash                 | String   | Resource pack hash   |              |
+----------------------+----------+----------------------+--------------+

### Respawn
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Dimension            | NBT      | Dimension            | 0x4C         |
| World Name           | String   | World name           |              |
| Hashed Seed          | Long     | Hashed seed          |              |
| Game Mode            | UByte    | Game mode            |              |
| Previous Game Mode   | Byte     | Previous game mode   |              |
| Is Debug             | Boolean  | Debug world flag     |              |
| Is Flat              | Boolean  | Flat world flag      |              |
| Copy Metadata        | Boolean  | Copy metadata flag   |              |
+----------------------+----------+----------------------+--------------+

### Set Head Rotation
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x4D         |
| Head Yaw             | Angle    | Head yaw             |              |
+----------------------+----------+----------------------+--------------+

### Update Section Blocks
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Section Position     | Long     | Section position     | 0x4E         |
| Trust Edges          | Boolean  | Trust edges flag     |              |
| Blocks               | VarLong[]| List of block states |              |
+----------------------+----------+----------------------+--------------+

### Select Advancements Tab
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Identifier           | String   | Tab identifier       | 0x4F         |
+----------------------+----------+----------------------+--------------+

### Server Data
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| MOTD                 | String   | Message of the day   | 0x50         |
| Icon                 | String   | Server icon          |              |
| Enforces Secure Chat | Boolean  | Enforces secure chat |              |
+----------------------+----------+----------------------+--------------+

### Set Action Bar Text
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Text                 | String   | Action bar text      | 0x51         |
+----------------------+----------+----------------------+--------------+

### Set Border Center
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| X                    | Double   | X coordinate         | 0x52         |
| Z                    | Double   | Z coordinate         |              |
+----------------------+----------+----------------------+--------------+

### Set Border Lerp Size
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Old Size             | Double   | Old size             | 0x53         |
| New Size             | Double   | New size             |              |
| Speed                | VarLong  | Speed                |              |
+----------------------+----------+----------------------+--------------+

### Set Border Size
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Size                 | Double   | Border size          | 0x54         |
+----------------------+----------+----------------------+--------------+

### Set Border Warning Delay
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Warning Time         | VarInt   | Warning time         | 0x55         |
+----------------------+----------+----------------------+--------------+

### Set Border Warning Distance
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Warning Blocks       | VarInt   | Warning blocks       | 0x56         |
+----------------------+----------+----------------------+--------------+

### Set Camera
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Camera ID            | VarInt   | Camera entity ID     | 0x57         |
+----------------------+----------+----------------------+--------------+

### Set Center Chunk
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Chunk X              | VarInt   | Chunk X coordinate   | 0x58         |
| Chunk Z              | VarInt   | Chunk Z coordinate   |              |
+----------------------+----------+----------------------+--------------+

### Set Render Distance
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| View Distance        | VarInt   | View distance        | 0x59         |
+----------------------+----------+----------------------+--------------+

### Set Cursor Item
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Slot Data            | Slot     | Cursor item data     | 0x5A         |
+----------------------+----------+----------------------+--------------+

### Set Default Spawn Position
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Spawn position       | 0x5B         |
| Angle                | Float    | Spawn angle          |              |
+----------------------+----------+----------------------+--------------+

### Display Objective
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Position             | Byte     | Display position     | 0x5C         |
| Name                 | String   | Objective name       |              |
+----------------------+----------+----------------------+--------------+

### Set Entity Metadata
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x5D         |
| Metadata             | Metadata[]| List of metadata    |              |
+----------------------+----------+----------------------+--------------+

### Link Entities
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Vehicle ID           | VarInt   | Vehicle entity ID    | 0x5E         |
| Passenger IDs        | VarInt[] | List of passenger IDs|              |
+----------------------+----------+----------------------+--------------+

### Set Entity Velocity
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x5F         |
| Velocity X           | Short    | X velocity           |              |
| Velocity Y           | Short    | Y velocity           |              |
| Velocity Z           | Short    | Z velocity           |              |
+----------------------+----------+----------------------+--------------+

### Set Equipment
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x60         |
| Equipment            | Equipment[]| List of equipment  |              |
+----------------------+----------+----------------------+--------------+

### Set Experience
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Experience Bar       | Float    | Experience bar value | 0x61         |
| Level                | VarInt   | Player level         |              |
| Total Experience     | VarInt   | Total experience     |              |
+----------------------+----------+----------------------+--------------+

### Set Health
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Health               | Float    | Player health        | 0x62         |
| Food                 | VarInt   | Food level           |              |
| Food Saturation      | Float    | Food saturation      |              |
+----------------------+----------+----------------------+--------------+

### Set Held Item (clientbound)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Slot                 | Byte     | Held item slot       | 0x63         |
+----------------------+----------+----------------------+--------------+

### Update Objectives
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Name                 | String   | Objective name       | 0x64         |
| Mode                 | Byte     | Update mode          |              |
| Display Name         | String   | Display name         |              |
| Type                 | VarInt   | Objective type       |              |
+----------------------+----------+----------------------+--------------+

### Set Passengers
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Vehicle entity ID    | 0x65         |
| Passenger IDs        | VarInt[] | List of passenger IDs|              |
+----------------------+----------+----------------------+--------------+

### Set Player Inventory Slot
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Slot                 | Short    | Inventory slot       | 0x66         |
| Slot Data            | Slot     | Slot data            |              |
+----------------------+----------+----------------------+--------------+

### Update Teams
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Team Name            | String   | Team name            | 0x67         |
| Mode                 | Byte     | Update mode          |              |
| Display Name         | String   | Display name         |              |
| Prefix               | String   | Team prefix          |              |
| Suffix               | String   | Team suffix          |              |
| Flags                | Byte     | Team flags           |              |
| Name Tag Visibility  | String   | Name tag visibility  |              |
| Collision Rule       | String   | Collision rule       |              |
| Color                | VarInt   | Team color           |              |
| Players              | String[] | List of players      |              |
+----------------------+----------+----------------------+--------------+

### Update Score
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity Name          | String   | Entity name          | 0x68         |
| Action               | Byte     | Update action        |              |
| Objective Name       | String   | Objective name       |              |
| Value                | VarInt   | Score value          |              |
+----------------------+----------+----------------------+--------------+

### Set Simulation Distance
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Simulation Distance  | VarInt   | Simulation distance  | 0x69         |
+----------------------+----------+----------------------+--------------+

### Set Subtitle Text
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Text                 | String   | Subtitle text        | 0x6A         |
+----------------------+----------+----------------------+--------------+

### Update Time
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| World Age            | Long     | World age            | 0x6B         |
| Time of Day          | Long     | Time of day          |              |
+----------------------+----------+----------------------+--------------+

### Set Title Text
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Text                 | String   | Title text           | 0x6C         |
+----------------------+----------+----------------------+--------------+

### Set Title Animation Times
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Fade In              | Int      | Fade in time         | 0x6D         |
| Stay                 | Int      | Stay time            |              |
| Fade Out             | Int      | Fade out time        |              |
+----------------------+----------+----------------------+--------------+

### Entity Sound Effect
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Sound ID             | VarInt   | Sound ID             | 0x6E         |
| Sound Category       | VarInt   | Sound category       |              |
| Entity ID            | VarInt   | Unique entity ID     |              |
| Volume               | Float    | Sound volume         |              |
| Pitch                | Float    | Sound pitch          |              |
+----------------------+----------+----------------------+--------------+

### Sound Effect
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Sound ID             | VarInt   | Sound ID             | 0x6F         |
| Sound Category       | VarInt   | Sound category       |              |
| X                    | Int      | X coordinate         |              |
| Y                    | Int      | Y coordinate         |              |
| Z                    | Int      | Z coordinate         |              |
| Volume               | Float    | Sound volume         |              |
| Pitch                | Float    | Sound pitch          |              |
+----------------------+----------+----------------------+--------------+

### Start Configuration
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x70         |
+----------------------+----------+----------------------+--------------+

### Stop Sound
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Flags                | Byte     | Stop sound flags     | 0x71         |
| Source               | VarInt   | Sound source         |              |
| Sound                | VarInt   | Sound ID             |              |
+----------------------+----------+----------------------+--------------+

### Store Cookie (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Key                  | String   | Cookie identifier    | 0x72         |
| Value                | String   | Cookie value         |              |
+----------------------+----------+----------------------+--------------+

### System Chat Message
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Message              | String   | Chat message         | 0x73         |
| Type                 | Byte     | Message type         |              |
+----------------------+----------+----------------------+--------------+

### Set Tab List Header And Footer
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Header               | String   | Tab list header      | 0x74         |
| Footer               | String   | Tab list footer      |              |
+----------------------+----------+----------------------+--------------+

### Tag Query Response
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Transaction ID       | VarInt   | Transaction ID       | 0x75         |
| NBT                  | NBT      | Tag data             |              |
+----------------------+----------+----------------------+--------------+

### Pickup Item
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Collected Entity ID  | VarInt   | Collected entity ID  | 0x76         |
| Collector Entity ID  | VarInt   | Collector entity ID  |              |
| Pickup Count         | VarInt   | Number of items picked up |         |
+----------------------+----------+----------------------+--------------+

### Synchronize Vehicle Position
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| X                    | Double   | X coordinate         | 0x77         |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Yaw                  | Float    | Yaw                  |              |
| Pitch                | Float    | Pitch                |              |
+----------------------+----------+----------------------+--------------+

### Set Ticking State
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Ticking position     | 0x78         |
| State                | Byte     | Ticking state        |              |
+----------------------+----------+----------------------+--------------+

### Step Tick
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Ticks                | VarInt   | Number of ticks      | 0x79         |
+----------------------+----------+----------------------+--------------+

### Transfer (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Server Address       | String   | Server address       | 0x7A         |
| Server Port          | UShort   | Server port          |              |
+----------------------+----------+----------------------+--------------+

### Update Advancements
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Reset                | Boolean  | Reset flag           | 0x7B         |
| Advancements         | Byte[]   | Advancements data    |              |
| Progress             | Byte[]   | Progress data        |              |
+----------------------+----------+----------------------+--------------+

### Update Attributes
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x7C         |
| Attributes           | Attribute[]| List of attributes |              |
+----------------------+----------+----------------------+--------------+

### Entity Effect
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x7D         |
| Effect ID            | Byte     | Effect ID            |              |
| Amplifier            | Byte     | Effect amplifier     |              |
| Duration             | VarInt   | Effect duration      |              |
| Flags                | Byte     | Effect flags         |              |
+----------------------+----------+----------------------+--------------+

### Update Recipes
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Recipes              | Byte[]   | Recipes data         | 0x7E         |
+----------------------+----------+----------------------+--------------+

### Update Tags (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Tags                 | Byte[]   | Tags data            | 0x7F         |
+----------------------+----------+----------------------+--------------+

### Projectile Power
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x80         |
| Power                | Float    | Projectile power     |              |
+----------------------+----------+----------------------+--------------+

### Custom Report Details
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Details              | Byte[]   | Report details       | 0x81         |
+----------------------+----------+----------------------+--------------+

### Server Links
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Links                | Byte[]   | Server links data    | 0x82         |
+----------------------+----------+----------------------+--------------+

## Serverbound

### Confirm Teleportation
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Teleport ID          | VarInt   | Teleport ID          | 0x00         |
+----------------------+----------+----------------------+--------------+

### Query Block Entity Tag
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Transaction ID       | VarInt   | Transaction ID       | 0x01         |
| Location             | Position | Block position       |              |
+----------------------+----------+----------------------+--------------+

### Bundle Item Selected
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Slot                 | VarInt   | Selected slot        | 0x02         |
+----------------------+----------+----------------------+--------------+

### Change Difficulty
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Difficulty           | UByte    | Difficulty level     | 0x03         |
+----------------------+----------+----------------------+--------------+

### Acknowledge Message
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Message ID           | UUID     | Message UUID         | 0x04         |
+----------------------+----------+----------------------+--------------+

### Chat Command
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Command              | String   | Chat command         | 0x05         |
| Timestamp            | Long     | Command timestamp    |              |
| Salt                 | Long     | Command salt         |              |
| Argument Signatures  | Byte[]   | Argument signatures  |              |
| Signed Preview       | Boolean  | Signed preview flag  |              |
| Last Seen Messages   | UUID[]   | List of last seen messages |        |
+----------------------+----------+----------------------+--------------+

### Signed Chat Command
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Command              | String   | Chat command         | 0x06         |
| Timestamp            | Long     | Command timestamp    |              |
| Salt                 | Long     | Command salt         |              |
| Argument Signatures  | Byte[]   | Argument signatures  |              |
| Signed Preview       | Boolean  | Signed preview flag  |              |
| Last Seen Messages   | UUID[]   | List of last seen messages |        |
+----------------------+----------+----------------------+--------------+

### Chat Message
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Message              | String   | Chat message         | 0x07         |
| Timestamp            | Long     | Message timestamp    |              |
| Salt                 | Long     | Message salt         |              |
| Signature            | Byte[]   | Message signature    |              |
| Signed Preview       | Boolean  | Signed preview flag  |              |
| Last Seen Messages   | UUID[]   | List of last seen messages |        |
+----------------------+----------+----------------------+--------------+

### Player Session
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Session ID           | UUID     | Player session ID    | 0x08         |
| Public Key           | Byte[]   | Public key           |              |
| Signature            | Byte[]   | Session signature    |              |
+----------------------+----------+----------------------+--------------+

### Chunk Batch Received
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x09         |
+----------------------+----------+----------------------+--------------+

### Client Status
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Action ID            | VarInt   | Client action ID     | 0x0A         |
+----------------------+----------+----------------------+--------------+

### Client Tick End
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x0B         |
+----------------------+----------+----------------------+--------------+

### Client Information (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Info                 | Byte[]   | Client information   | 0x0C         |
+----------------------+----------+----------------------+--------------+

### Command Suggestions Request
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | VarInt   | Suggestions ID       | 0x0D         |
| Text                 | String   | Command text         |              |
+----------------------+----------+----------------------+--------------+

### Acknowledge Configuration
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x0E         |
+----------------------+----------+----------------------+--------------+

### Click Container Button
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x0F         |
| Button ID            | UByte    | Button ID            |              |
+----------------------+----------+----------------------+--------------+

### Click Container
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x10         |
| State ID             | VarInt   | State ID             |              |
| Slot                 | Short    | Slot ID              |              |
| Button               | Byte     | Button ID            |              |
| Mode                 | VarInt   | Click mode           |              |
| Slots                | Slot[]   | List of slots        |              |
+----------------------+----------+----------------------+--------------+

### Close Container
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x11         |
+----------------------+----------+----------------------+--------------+

### Change Container Slot State
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x12         |
| Slot                 | Short    | Slot ID              |              |
| State                | Byte     | Slot state           |              |
+----------------------+----------+----------------------+--------------+

### Cookie Response (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Key                  | String   | Cookie identifier    | 0x13         |
| Value                | String   | Cookie value         |              |
+----------------------+----------+----------------------+--------------+

### Serverbound Plugin Message (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Channel              | String   | Plugin channel name  | 0x14         |
| Data                 | Byte[]   | Plugin data          |              |
+----------------------+----------+----------------------+--------------+

### Debug Sample Subscription
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Data                 | Byte[]   | Debug sample data    | 0x15         |
+----------------------+----------+----------------------+--------------+

### Edit Book
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Hand                 | VarInt   | Hand (0: main, 1: off)| 0x16        |
| Pages                | String[] | List of pages        |              |
| Title                | String   | Book title           |              |
+----------------------+----------+----------------------+--------------+

### Query Entity Tag
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Transaction ID       | VarInt   | Transaction ID       | 0x17         |
| Entity ID            | VarInt   | Unique entity ID     |              |
+----------------------+----------+----------------------+--------------+

### Interact
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x18         |
| Type                 | VarInt   | Interaction type     |              |
| Target X             | Float    | Target X coordinate  |              |
| Target Y             | Float    | Target Y coordinate  |              |
| Target Z             | Float    | Target Z coordinate  |              |
| Hand                 | VarInt   | Hand (0: main, 1: off)|             |
| Sneaking             | Boolean  | Sneaking flag        |              |
+----------------------+----------+----------------------+--------------+

### Jigsaw Generate
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Jigsaw block position| 0x19         |
| Levels               | VarInt   | Number of levels     |              |
| Keep Jigsaws         | Boolean  | Keep jigsaws flag    |              |
+----------------------+----------+----------------------+--------------+

### Serverbound Keep Alive (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Keep alive ID        | 0x1A         |
+----------------------+----------+----------------------+--------------+

### Lock Difficulty
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Locked               | Boolean  | Lock difficulty flag | 0x1B         |
+----------------------+----------+----------------------+--------------+

### Set Player Position
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| X                    | Double   | X coordinate         | 0x1C         |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Yaw                  | Float    | Yaw                  |              |
| Pitch                | Float    | Pitch                |              |
| Flags                | Byte     | Position flags       |              |
| Teleport ID          | VarInt   | Teleport ID          |              |
+----------------------+----------+----------------------+--------------+

### Set Player Position and Rotation
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| X                    | Double   | X coordinate         | 0x1D         |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Yaw                  | Float    | Yaw                  |              |
| Pitch                | Float    | Pitch                |              |
| Flags                | Byte     | Position flags       |              |
| Teleport ID          | VarInt   | Teleport ID          |              |
+----------------------+----------+----------------------+--------------+

### Set Player Rotation
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Yaw                  | Float    | Yaw                  | 0x1E         |
| Pitch                | Float    | Pitch                |              |
| On Ground            | Boolean  | On ground flag       |              |
+----------------------+----------+----------------------+--------------+

### Set Player Movement Flags
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Flags                | Byte     | Movement flags       | 0x1F         |
+----------------------+----------+----------------------+--------------+

### Move Vehicle
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| X                    | Double   | X coordinate         | 0x20         |
| Y                    | Double   | Y coordinate         |              |
| Z                    | Double   | Z coordinate         |              |
| Yaw                  | Float    | Yaw                  |              |
| Pitch                | Float    | Pitch                |              |
+----------------------+----------+----------------------+--------------+

### Paddle Boat
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Left Paddle Turning  | Boolean  | Left paddle turning  | 0x21         |
| Right Paddle Turning | Boolean  | Right paddle turning |              |
+----------------------+----------+----------------------+--------------+

### Pick Item From Block
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Block position       | 0x22         |
+----------------------+----------+----------------------+--------------+

### Pick Item From Entity
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x23         |
+----------------------+----------+----------------------+--------------+

### Ping Request (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Ping ID              | 0x24         |
+----------------------+----------+----------------------+--------------+

### Place Recipe
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Window ID            | UByte    | Window ID            | 0x25         |
| Recipe               | String   | Recipe ID            |              |
| Make All             | Boolean  | Make all flag        |              |
+----------------------+----------+----------------------+--------------+

### Player Abilities (serverbound)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Flags                | Byte     | Ability flags        | 0x26         |
+----------------------+----------+----------------------+--------------+

### Player Action
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Action ID            | VarInt   | Player action ID     | 0x27         |
| Location             | Position | Action location      |              |
| Face                 | Byte     | Block face           |              |
| Sequence             | VarInt   | Action sequence      |              |
+----------------------+----------+----------------------+--------------+

### Player Command
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x28         |
| Action ID            | VarInt   | Player command ID    |              |
| Data                 | Int      | Command data         |              |
+----------------------+----------+----------------------+--------------+

### Player Input
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Sideways             | Float    | Sideways movement    | 0x29         |
| Forward              | Float    | Forward movement     |              |
| Flags                | Byte     | Input flags          |              |
+----------------------+----------+----------------------+--------------+

### Player Loaded
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | -                    | 0x2A         |
+----------------------+----------+----------------------+--------------+

### Pong (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| ID                   | Long     | Pong ID              | 0x2B         |
+----------------------+----------+----------------------+--------------+

### Change Recipe Book Settings
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Book ID              | VarInt   | Recipe book ID       | 0x2C         |
| Book Open            | Boolean  | Book open flag       |              |
| Filter Active        | Boolean  | Filter active flag   |              |
+----------------------+----------+----------------------+--------------+

### Set Seen Recipe
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Recipe               | String   | Recipe ID            | 0x2D         |
+----------------------+----------+----------------------+--------------+

### Rename Item
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Name                 | String   | New item name        | 0x2E         |
+----------------------+----------+----------------------+--------------+

### Resource Pack Response (play)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Status               | VarInt   | Response status      | 0x2F         |
+----------------------+----------+----------------------+--------------+

### Seen Advancements
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Action               | VarInt   | Action type          | 0x30         |
| Tab ID               | String   | Tab identifier       |              |
+----------------------+----------+----------------------+--------------+

### Select Trade
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Slot                 | VarInt   | Trade slot           | 0x31         |
+----------------------+----------+----------------------+--------------+

### Set Beacon Effect
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Primary Effect       | VarInt   | Primary effect       | 0x32         |
| Secondary Effect     | VarInt   | Secondary effect     |              |
+----------------------+----------+----------------------+--------------+

### Set Held Item (serverbound)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Slot                 | Byte     | Held item slot       | 0x33         |
+----------------------+----------+----------------------+--------------+

### Program Command Block
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Command block position| 0x34        |
| Command              | String   | Command              |              |
| Mode                 | VarInt   | Command block mode   |              |
| Flags                | Byte     | Command block flags  |              |
+----------------------+----------+----------------------+--------------+

### Program Command Block Minecart
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Entity ID            | VarInt   | Unique entity ID     | 0x35         |
| Command              | String   | Command              |              |
| Track Output         | Boolean  | Track output flag    |              |
+----------------------+----------+----------------------+--------------+

### Set Creative Mode Slot
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Slot                 | Short    | Inventory slot       | 0x36         |
| Slot Data            | Slot     | Slot data            |              |
+----------------------+----------+----------------------+--------------+

### Program Jigsaw Block
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Jigsaw block position| 0x37         |
| Name                 | String   | Jigsaw block name    |              |
| Target               | String   | Jigsaw block target  |              |
| Pool                 | String   | Jigsaw block pool    |              |
| Final State          | String   | Final state          |              |
| Joint Type           | String   | Joint type           |              |
+----------------------+----------+----------------------+--------------+

### Program Structure Block
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Structure block position| 0x38      |
| Action               | VarInt   | Structure block action|              |
| Mode                 | VarInt   | Structure block mode |              |
| Name                 | String   | Structure name       |              |
| Offset X             | Byte     | X offset             |              |
| Offset Y             | Byte     | Y offset             |              |
| Offset Z             | Byte     | Z offset             |              |
| Size X               | Byte     | X size               |              |
| Size Y               | Byte     | Y size               |              |
| Size Z               | Byte     | Z size               |              |
| Mirror               | VarInt   | Mirror type          |              |
| Rotation             | VarInt   | Rotation type        |              |
| Metadata             | String   | Metadata             |              |
| Integrity            | Float    | Integrity            |              |
| Seed                 | VarLong  | Seed                 |              |
| Flags                | Byte     | Structure block flags|              |
+----------------------+----------+----------------------+--------------+

### Update Sign
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Location             | Position | Sign position        | 0x39         |
| Is Front Text        | Boolean  | Is front text flag   |              |
| Text 1               | String   | Line 1 text          |              |
| Text 2               | String   | Line 2 text          |              |
| Text 3               | String   | Line 3 text          |              |
| Text 4               | String   | Line 4 text          |              |
+----------------------+----------+----------------------+--------------+

### Swing Arm
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Hand                 | VarInt   | Hand (0: main, 1: off)| 0x3A        |
+----------------------+----------+----------------------+--------------+

### Teleport To Entity
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Target Entity ID     | VarInt   | Target entity ID     | 0x3B         |
+----------------------+----------+----------------------+--------------+

### Use Item On
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Hand                 | VarInt   | Hand (0: main, 1: off)| 0x3C        |
| Location             | Position | Block position       |              |
| Face                 | VarInt   | Block face           |              |
| Cursor X             | Float    | Cursor X coordinate  |              |
| Cursor Y             | Float    | Cursor Y coordinate  |              |
| Cursor Z             | Float    | Cursor Z coordinate  |              |
| Inside Block         | Boolean  | Inside block flag    |              |
+----------------------+----------+----------------------+--------------+
