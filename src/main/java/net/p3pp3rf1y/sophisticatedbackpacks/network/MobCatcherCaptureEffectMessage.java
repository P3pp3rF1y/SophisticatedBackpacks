package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.MobCatcherCaptureEffectRenderer;

import java.util.function.Supplier;

public record MobCatcherCaptureEffectMessage(ResourceLocation entityType, CompoundTag entityNbt, Vec3 position, Vec3 collapsePosition, float yRot, float xRot) {
	public static void encode(MobCatcherCaptureEffectMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeResourceLocation(msg.entityType);
		packetBuffer.writeNbt(msg.entityNbt);
		writeVec3(packetBuffer, msg.position);
		writeVec3(packetBuffer, msg.collapsePosition);
		packetBuffer.writeFloat(msg.yRot);
		packetBuffer.writeFloat(msg.xRot);
	}

	public static MobCatcherCaptureEffectMessage decode(FriendlyByteBuf packetBuffer) {
		ResourceLocation entityType = packetBuffer.readResourceLocation();
		CompoundTag entityNbt = packetBuffer.readAnySizeNbt();
		return new MobCatcherCaptureEffectMessage(entityType, entityNbt == null ? new CompoundTag() : entityNbt, readVec3(packetBuffer), readVec3(packetBuffer), packetBuffer.readFloat(), packetBuffer.readFloat());
	}

	public static void onMessage(MobCatcherCaptureEffectMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> MobCatcherCaptureEffectRenderer.addEffect(msg.entityType, msg.entityNbt, msg.position, msg.collapsePosition, msg.yRot, msg.xRot));
		context.setPacketHandled(true);
	}

	private static void writeVec3(FriendlyByteBuf packetBuffer, Vec3 vec) {
		packetBuffer.writeDouble(vec.x);
		packetBuffer.writeDouble(vec.y);
		packetBuffer.writeDouble(vec.z);
	}

	private static Vec3 readVec3(FriendlyByteBuf packetBuffer) {
		return new Vec3(packetBuffer.readDouble(), packetBuffer.readDouble(), packetBuffer.readDouble());
	}
}
