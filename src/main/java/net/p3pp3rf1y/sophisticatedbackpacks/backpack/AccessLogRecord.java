package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ResourceLocation;

import java.util.UUID;

public class AccessLogRecord {
	private final ResourceLocation backpackItemRegistryName;
	private final UUID backpackUuid;
	private final String playerName;
	private final String backpackName;
	private final int clothColor;
	private final int trimColor;
	private final long accessTime;

	public AccessLogRecord(ResourceLocation backpackItemRegistryName, UUID backpackUuid, String playerName, String backpackName, int clothColor, int trimColor, long accessTime) {
		this.backpackItemRegistryName = backpackItemRegistryName;
		this.backpackUuid = backpackUuid;
		this.playerName = playerName;
		this.backpackName = backpackName;
		this.clothColor = clothColor;
		this.trimColor = trimColor;
		this.accessTime = accessTime;
	}

	public UUID getBackpackUuid() {
		return backpackUuid;
	}

	public String getPlayerName() {
		return playerName;
	}

	public String getBackpackName() {
		return backpackName;
	}

	public int getClothColor() {
		return clothColor;
	}

	public int getTrimColor() {
		return trimColor;
	}

	public long getAccessTime() {
		return accessTime;
	}

	public ResourceLocation getBackpackItemRegistryName() {
		return backpackItemRegistryName;
	}

	public CompoundNBT serializeToNBT() {
		CompoundNBT ret = new CompoundNBT();
		ret.putString("backpackItemRegistryName", backpackItemRegistryName.toString());
		ret.putUniqueId("backpackUuid", backpackUuid);
		ret.putString("playerName", playerName);
		ret.putString("backpackName", backpackName);
		ret.putInt("clothColor", clothColor);
		ret.putInt("trimColor", trimColor);
		ret.putLong("accessTime", accessTime);
		return ret;
	}

	public static AccessLogRecord deserializeFromNBT(CompoundNBT nbt) {
		return new AccessLogRecord(
				new ResourceLocation(nbt.getString("backpackItemRegistryName")),
				nbt.getUniqueId("backpackUuid"),
				nbt.getString("playerName"),
				nbt.getString("backpackName"),
				nbt.getInt("clothColor"),
				nbt.getInt("trimColor"),
				nbt.getLong("accessTime")
		);
	}
}
