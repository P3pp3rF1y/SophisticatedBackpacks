package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeRenderDataType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

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
	public CompoundNBT serializeNBT() {
		return NBTHelper.putBoolean(new CompoundNBT(), "playing", playing);
	}

	public static JukeboxUpgradeRenderData deserializeNBT(CompoundNBT nbt) {
		return new JukeboxUpgradeRenderData(nbt.getBoolean("playing"));
	}
}
