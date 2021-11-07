package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import com.mojang.brigadier.StringReader;
import com.mojang.serialization.Codec;
import net.minecraft.network.PacketBuffer;
import net.minecraft.particles.IParticleData;
import net.minecraft.particles.ParticleType;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModParticles;

public class JukeboxUpgradeNoteParticleData extends ParticleType<JukeboxUpgradeNoteParticleData> implements IParticleData {
	public JukeboxUpgradeNoteParticleData() {
		super(false, DESERIALIZER);
	}

	@Override
	public JukeboxUpgradeNoteParticleData getType() {
		return ModParticles.JUKEBOX_NOTE.get();
	}

	@Override
	public void writeToNetwork(PacketBuffer pBuffer) {
		//noop
	}

	@Override
	public String writeToString() {
		//noinspection ConstantConditions
		return ModParticles.JUKEBOX_NOTE.get().getRegistryName().toString();
	}

	public static final IDeserializer<JukeboxUpgradeNoteParticleData> DESERIALIZER = new IDeserializer<JukeboxUpgradeNoteParticleData>() {
		@Override
		public JukeboxUpgradeNoteParticleData fromCommand(ParticleType<JukeboxUpgradeNoteParticleData> pParticleType, StringReader pReader) {
			return (JukeboxUpgradeNoteParticleData) pParticleType;
		}

		@Override
		public JukeboxUpgradeNoteParticleData fromNetwork(ParticleType<JukeboxUpgradeNoteParticleData> pParticleType, PacketBuffer pBuffer) {
			return (JukeboxUpgradeNoteParticleData) pParticleType;
		}
	};

	private final Codec<JukeboxUpgradeNoteParticleData> codec = Codec.unit(this::getType);

	@Override
	public Codec<JukeboxUpgradeNoteParticleData> codec() {
		return codec;
	}
}
