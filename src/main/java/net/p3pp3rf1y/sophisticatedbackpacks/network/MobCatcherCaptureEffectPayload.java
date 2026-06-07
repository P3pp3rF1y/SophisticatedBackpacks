package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.phys.Vec3;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.MobCatcherCaptureEffectRenderer;
import net.p3pp3rf1y.sophisticatedcore.util.StreamCodecHelper;

public record MobCatcherCaptureEffectPayload(ResourceLocation entityType, CompoundTag entityNbt, Vec3 position, Vec3 collapsePosition, float yRot,
			float xRot) implements CustomPacketPayload {
	public static final Type<MobCatcherCaptureEffectPayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("mob_catcher_capture_effect"));
	public static final StreamCodec<ByteBuf, MobCatcherCaptureEffectPayload> STREAM_CODEC = StreamCodec.composite(
			ResourceLocation.STREAM_CODEC,
			MobCatcherCaptureEffectPayload::entityType,
			ByteBufCodecs.COMPOUND_TAG,
			MobCatcherCaptureEffectPayload::entityNbt,
			StreamCodecHelper.VEC3,
			MobCatcherCaptureEffectPayload::position,
			StreamCodecHelper.VEC3,
			MobCatcherCaptureEffectPayload::collapsePosition,
			ByteBufCodecs.FLOAT,
			MobCatcherCaptureEffectPayload::yRot,
			ByteBufCodecs.FLOAT,
			MobCatcherCaptureEffectPayload::xRot,
			MobCatcherCaptureEffectPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(MobCatcherCaptureEffectPayload payload, IPayloadContext context) {
		MobCatcherCaptureEffectRenderer.addEffect(payload.entityType, payload.entityNbt, payload.position, payload.collapsePosition, payload.yRot, payload.xRot);
	}
}
