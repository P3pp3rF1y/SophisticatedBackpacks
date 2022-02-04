package net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedcore.renderdata.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.UpgradeRenderDataType;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

public class JukeboxUpgradeRenderData implements IUpgradeRenderData {
	public static final UpgradeRenderDataType<JukeboxUpgradeRenderData> TYPE = new UpgradeRenderDataType<>("jukebox", JukeboxUpgradeRenderData.class, JukeboxUpgradeRenderData::deserializeNBT);

	private final boolean playing;

	public JukeboxUpgradeRenderData(boolean playing) {
		this.playing = playing;
	}

	public boolean isPlaying() {
		return playing;
	}

	@Override
	public CompoundTag serializeNBT() {
		return NBTHelper.putBoolean(new CompoundTag(), "playing", playing);
	}

	public static JukeboxUpgradeRenderData deserializeNBT(CompoundTag nbt) {
		return new JukeboxUpgradeRenderData(nbt.getBoolean("playing"));
	}
}
